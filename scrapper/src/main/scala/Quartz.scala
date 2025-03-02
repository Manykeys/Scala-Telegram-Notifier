import Clients.{CommentsClientFactory, GithubComment, GithubCommentsResponse, StackOverflowCommentsResponse}
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*
import io.circe.generic.auto.*
import io.circe.parser.decode
import org.quartz.*
import org.quartz.impl.StdSchedulerFactory
import scrapper.Endpoints.{AddNumberRequest, LinksDataResponse, NumberResponse}
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import sttp.client3.{UriContext, asStringAlways, basicRequest, emptyRequest}
import sttp.model.StatusCode
import tethys.JsonWriterOps
import tethys.jackson.*

import java.time.Instant
import scala.util.Try

case class Comment(created_at: String)

class GithubJob extends Job {

  override def execute(context: JobExecutionContext): Unit = {
    processLinks.handleErrorWith { err =>
      IO.println(s"Ошибка выполнения GithubJob: ${err.getMessage}")
    }.unsafeRunSync()
  }

  private def getToken: IO[String] = IO.fromOption(sys.env.get("GITHUB_TOKEN"))(
    new RuntimeException("Переменная окружения GITHUB_TOKEN не найдена")
  )

  private def getLinks: IO[LinksDataResponse] = {
    HttpClientCatsBackend.resource[IO]().use { backend =>
      val request = emptyRequest
        .get(uri"http://localhost:8080/links/all")
        .response(asStringAlways)
      for {
        live         <- backend.send(request)
        responseBody  = live.body
        response     <- IO {
          live.code match {
            case StatusCode.Ok =>
              decode[LinksDataResponse](responseBody) match {
                case Right(linksData) =>
                  println(s"Получены данные: ${linksData.data.keys.mkString(", ")}")
                  linksData
                case Left(err) =>
                  println(s"Ошибка парсинга JSON: $err")
                  LinksDataResponse(Map.empty)
              }
            case status =>
              println(s"Ошибка запроса: статус ${status.code}, тело ответа: $responseBody")
              LinksDataResponse(Map.empty)
          }
        }
      } yield response
    }.handleErrorWith { error =>
      IO.println(s"Ошибка при выполнении запроса: ${error.getMessage}") *> IO.pure(LinksDataResponse(Map.empty))
    }
  }

  private def fetchCurrentNumber(identifier: String): IO[Option[NumberResponse]] = {
    HttpClientCatsBackend.resource[IO]().use { backend =>
      val req = basicRequest
        .get(uri"http://localhost:8080/number/$identifier")
        .response(asStringAlways)
      backend.send(req).flatMap { resp =>
        if (resp.code == StatusCode.Ok)
          IO.fromEither(decode[NumberResponse](resp.body)
              .leftMap(err => new Exception(s"Decode NumberResponse error: $err")))
            .map(Some(_))
        else IO.pure(None)
      }
    }
  }

  private def updateNumberAndNotify(identifier: String, newTimestamp: Long, linksData: LinksDataResponse): IO[Unit] = {
    HttpClientCatsBackend.resource[IO]().use { backend =>
      val updateReq = basicRequest
        .post(uri"http://localhost:8080/number")
        .body(AddNumberRequest(identifier, newTimestamp).asJson)
        .response(asStringAlways)
      backend.send(updateReq).flatMap { resp =>
        IO.println(s"Обновлено число для $identifier: ответ: ${resp.body}")
      }
    } *> {
      linksData.data.get(identifier) match {
        case Some(chatIds) =>
          HttpClientCatsBackend.resource[IO]().use { backend =>
            val arrayChatIds = chatIds.mkString("[", ", ", "]")
            val jsonBody = s"""{"id": ${chatIds.head}, "url": "$identifier","description": "string","tgChatsIds": $arrayChatIds}"""
            IO.println(jsonBody)
            val updateChatReq = basicRequest
              .post(uri"http://localhost:4040/updates")
              .body(jsonBody)
              .contentType("application/json")
              .response(asStringAlways)
            backend.send(updateChatReq).flatMap { resp =>
              IO.println(s"Отправлено обновление для chatId $chatIds: ответ: ${resp.body}")
            }
          }
        case None =>
          IO.println(s"Для репозитория $identifier не найдены chatId")
      }
    }
  }

  private def processGithubComments(identifier: String, comments: List[GithubComment], linksData: LinksDataResponse): IO[Unit] = {
    comments.lastOption match {
      case Some(comment) =>
        for {
          newTimestamp <- IO.fromEither(
            Try(Instant.parse(comment.created_at).toEpochMilli)
              .toEither
              .leftMap(ex => new Exception(s"Ошибка парсинга created_at для $identifier: ${ex.getMessage}"))
          )
          _ <- IO.println(s"Repo $identifier: новый timestamp = $newTimestamp")
          currentNumberResponseOpt <- fetchCurrentNumber(identifier)
          currentValue = currentNumberResponseOpt.map(_.value).getOrElse(0L)
          _ <- IO.println(s"Repo $identifier: текущее сохранённое значение = $currentValue")
          _ <- if (newTimestamp != currentValue) {
            updateNumberAndNotify(identifier, newTimestamp, linksData)
          } else IO.println(s"Для репозитория $identifier обновление не требуется")
        } yield ()
      case None =>
        IO.println(s"Для репозитория $identifier не найдено ни одного комментария")
    }
  }

  private def processSingleLink(identifier: String, token: String, linksData: LinksDataResponse): IO[Unit] = {
    val commentsClient = CommentsClientFactory.create[IO](token, identifier)
    commentsClient.getComments(identifier, Some(0)).flatMap {
      case GithubCommentsResponse(comments) =>
        IO.println("shit") *> processGithubComments(identifier, comments, linksData)
      case StackOverflowCommentsResponse(response) =>
        IO.println(s"StackOverflow response for $identifier: $response")
    }
  }

  private def processLinks: IO[Unit] = {
    for {
      token     <- getToken
      linksData <- getLinks
      _ <- IO.println("все ок")
      _         <- linksData.data.keys.toList.traverse_ { identifier =>
        processSingleLink(identifier, token, linksData)
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
