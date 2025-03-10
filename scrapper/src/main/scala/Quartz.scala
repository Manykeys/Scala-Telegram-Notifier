import Clients.{CommentsClientFactory, GithubComment, GithubCommentsResponse, StackOverflowCommentsResponse}
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*
import io.circe.generic.auto.*
import io.circe.parser.decode
import org.quartz.*
import org.quartz.impl.StdSchedulerFactory
import org.slf4j.{Logger, LoggerFactory}
import scrapper.Endpoints.{AddNumberRequest, LinksDataResponse, NumberResponse}
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import sttp.client3.{asStringAlways, basicRequest, emptyRequest}
import sttp.model.{StatusCode, Uri}
import tethys.JsonWriterOps
import tethys.jackson.*

import java.time.Instant
import scala.util.Try

case class Comment(created_at: String)

class GithubJob extends Job {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  private def getEnvInt(name: String): IO[Int] =
    IO(sys.env.get(name)).flatMap {
      case Some(value) =>
        IO.fromTry(Try(value.toInt)).handleErrorWith { _ =>
          IO.raiseError(new RuntimeException(s"Переменная окружения $name имеет некорректный формат: '$value'"))
        }
      case None =>
        IO.raiseError(new RuntimeException(s"Переменная окружения $name не найдена"))
    }

  private def getToken: IO[String] = IO.fromOption(sys.env.get("GITHUB_TOKEN"))(
    new RuntimeException("Переменная окружения GITHUB_TOKEN не найдена")
  )

  private def buildUri(str: String): IO[Uri] =
    IO.fromEither(Uri.parse(str).left.map(err => new RuntimeException(s"Некорректный URI '$str': $err")))

  override def execute(context: JobExecutionContext): Unit = {
    processLinks.handleErrorWith { err =>
      IO.raiseError(new Exception(s"Ошибка выполнения GithubJob: ${err.getMessage}"))
    }.unsafeRunSync()
  }

  private def getLinks(apiUri: Uri): IO[LinksDataResponse] = {
    HttpClientCatsBackend.resource[IO]().use { backend =>
      val request = emptyRequest
        .get(apiUri.addPath("links").addPath("all"))
        .response(asStringAlways)
      for {
        live <- backend.send(request)
        responseBody = live.body
        response <- IO {
          live.code match {
            case StatusCode.Ok =>
              decode[LinksDataResponse](responseBody) match {
                case Right(linksData) =>
                  logger.info(s"Получены данные: ${linksData.data.keys.mkString(", ")}")
                  linksData
                case Left(err) =>
                  logger.info(s"Ошибка парсинга JSON: $err")
                  LinksDataResponse(Map.empty)
              }
            case status =>
              IO.pure(logger.info(s"Ошибка запроса: статус ${status.code}, тело ответа: $responseBody"))
              LinksDataResponse(Map.empty)
          }
        }
      } yield response
    }.handleErrorWith { error =>
      IO.pure(logger.info(s"Ошибка при выполнении запроса: ${error.getMessage}")) *> IO.pure(
        LinksDataResponse(Map.empty)
      )
    }
  }

  private def fetchCurrentNumber(apiUri: Uri, identifier: String): IO[Option[NumberResponse]] = {
    HttpClientCatsBackend.resource[IO]().use { backend =>
      val uri = apiUri.addPath("number").addPath(identifier)
      val req = basicRequest.get(uri).response(asStringAlways)
      backend.send(req).flatMap { resp =>
        if (resp.code == StatusCode.Ok)
          IO.fromEither(decode[NumberResponse](resp.body)
            .leftMap(err => new Exception(s"Decode NumberResponse error: $err")))
            .map(Some(_))
        else IO.pure(None)
      }
    }
  }

  private def updateNumberAndNotify(
      apiUri: Uri,
      tgUri: Uri,
      identifier: String,
      newTimestamp: Long,
      linksData: LinksDataResponse
  ): IO[Unit] = {
    HttpClientCatsBackend.resource[IO]().use { backend =>
      val updateReq = basicRequest
        .post(apiUri.addPath("number"))
        .body(AddNumberRequest(identifier, newTimestamp).asJson)
        .response(asStringAlways)
      backend.send(updateReq).flatMap { resp =>
        IO.pure(logger.info(s"Обновлено число для $identifier: ответ: ${resp.body}"))
      }
    } *> {
      linksData.data.get(identifier) match {
        case Some(chatIds) =>
          HttpClientCatsBackend.resource[IO]().use { backend =>
            val arrayChatIds = chatIds.mkString("[", ", ", "]")
            val jsonBody =
              s"""{"id": ${chatIds.head}, "url": "$identifier","description": "string","tgChatsIds": $arrayChatIds}"""
            IO.pure(logger.info(jsonBody))
            val updateChatReq = basicRequest
              .post(tgUri.addPath("updates"))
              .body(jsonBody)
              .contentType("application/json")
              .response(asStringAlways)
            backend.send(updateChatReq).flatMap { resp =>
              IO.pure(logger.info(s"Отправлено обновление для chatId $chatIds: ответ: ${resp.body}"))
            }
          }
        case None =>
          IO.pure(logger.info(s"Для репозитория $identifier не найдены chatId"))
      }
    }
  }

  private def processGithubComments(
      apiUri: Uri,
      tgUri: Uri,
      identifier: String,
      comments: List[GithubComment],
      linksData: LinksDataResponse
  ): IO[Unit] = {
    comments.lastOption match {
      case Some(comment) =>
        for {
          newTimestamp <- IO.fromEither(
            Try(Instant.parse(comment.created_at).toEpochMilli)
              .toEither
              .leftMap(ex => new Exception(s"Ошибка парсинга created_at для $identifier: ${ex.getMessage}"))
          )
          _                        <- IO.pure(logger.info(s"Repo $identifier: новый timestamp = $newTimestamp"))
          currentNumberResponseOpt <- fetchCurrentNumber(apiUri, identifier)
          currentValue = currentNumberResponseOpt.map(_.value).getOrElse(0L)
          _ <- IO.pure(logger.info(s"Repo $identifier: текущее сохранённое значение = $currentValue"))
          _ <- if (newTimestamp != currentValue) {
            updateNumberAndNotify(apiUri, tgUri, identifier, newTimestamp, linksData)
          } else IO.pure(logger.info(s"Для репозитория $identifier обновление не требуется"))
        } yield ()
      case None =>
        IO.pure(logger.info(s"Для репозитория $identifier не найдено ни одного комментария"))
    }
  }

  private def processSingleLink(
      apiUri: Uri,
      tgUri: Uri,
      identifier: String,
      token: String,
      linksData: LinksDataResponse
  ): IO[Unit] = {
    HttpClientCatsBackend.resource[IO]().use { backend =>
      val commentsClient = CommentsClientFactory.create[IO](token, identifier, backend)

      commentsClient.getComments(identifier, Some(0)).flatMap {
        case GithubCommentsResponse(comments) =>
          processGithubComments(apiUri, tgUri, identifier, comments, linksData)
        case StackOverflowCommentsResponse(response) =>
          IO.pure(logger.info(s"StackOverflow response for $identifier: $response"))
      }
    }
  }

  private def processLinks: IO[Unit] = {
    for {
      apiPort   <- getEnvInt("APIPORT")
      tgPort    <- getEnvInt("TGPORT")
      apiUri    <- buildUri(s"http://localhost:$apiPort")
      tgUri     <- buildUri(s"http://localhost:$tgPort")
      token     <- getToken
      linksData <- getLinks(apiUri)
      _ <- linksData.data.keys.toList.traverse_ { identifier =>
        processSingleLink(apiUri, tgUri, identifier, token, linksData)
      }
    } yield ()
  }
}

object QuartzApp extends IOApp.Simple {

  def createScheduler: Resource[IO, Scheduler] = Resource.make {
    IO {
      val scheduler: Scheduler = StdSchedulerFactory.getDefaultScheduler
      scheduler.start()
      scheduler
    }
  } { scheduler =>
    IO(scheduler.shutdown())
  }

  override def run: IO[Unit] = {
    createScheduler.use { scheduler =>
      val jobDetail = JobBuilder.newJob(classOf[GithubJob])
        .withIdentity("GithubJob", "group1")
        .build()

      val trigger = TriggerBuilder.newTrigger()
        .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *"))
        .build()

      IO(scheduler.scheduleJob(jobDetail, trigger)) *> IO.never
    }
  }
}
