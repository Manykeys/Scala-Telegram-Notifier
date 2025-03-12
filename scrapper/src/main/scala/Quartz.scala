package scrapper

import Clients.{CommentsClientFactory, GithubComment, GithubCommentsResponse, StackOverflowCommentsResponse}
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.Port
import io.circe.generic.auto.*
import io.circe.parser.decode
import org.quartz.*
import org.quartz.impl.StdSchedulerFactory
import org.quartz.spi.{JobFactory, TriggerFiredBundle}
import org.slf4j.{Logger, LoggerFactory}
import pureconfig.ConfigSource
import scrapper.Endpoints.{AddNumberRequest, LinksDataResponse, NumberResponse}
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import sttp.client3.{SttpBackend, asStringAlways, basicRequest}
import sttp.model.{StatusCode, Uri}
import tethys.JsonWriterOps
import tethys.jackson.*

import java.time.Instant
import scala.util.Try

case class NumberUpdatePayload(identifier: String, timestamp: Long)

case class TelegramNotificationPayload(id: Long, url: String, description: String, tgChatsIds: List[Long])

sealed trait ProcessResult
case object NoComments                            extends ProcessResult
case class Updated(newTimestamp: Long)            extends ProcessResult
case class NoUpdateNeeded(currentTimestamp: Long) extends ProcessResult

trait NumberUpdater {
  def updateNumber(apiUri: Uri, payload: NumberUpdatePayload): IO[String]
}

trait TelegramNotifier {
  def notifyTelegram(tgUri: Uri, payload: TelegramNotificationPayload): IO[String]
}

class DefaultNumberUpdater(httpClient: SttpBackend[IO, Any]) extends NumberUpdater {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def buildPayload(identifier: String, timestamp: Long): NumberUpdatePayload =
    NumberUpdatePayload(identifier, timestamp)

  override def updateNumber(apiUri: Uri, payload: NumberUpdatePayload): IO[String] = {
    val req = basicRequest
      .post(apiUri.addPath("number"))
      .body(AddNumberRequest(payload.identifier, payload.timestamp).asJson)
      .response(asStringAlways)
    httpClient.send(req).flatMap { resp =>
      IO(logger.info(s"Обновление числа для ${payload.identifier} получило ответ: ${resp.body}")) *>
        IO.pure(resp.body)
    }
  }
}

class DefaultTelegramNotifier(httpClient: SttpBackend[IO, Any]) extends TelegramNotifier {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def notifyTelegram(tgUri: Uri, payload: TelegramNotificationPayload): IO[String] = {
    val jsonBody =
      s"""{"id": ${payload.id}, "url": "${payload.url}", "description": "${payload
          .description}", "tgChatsIds": ${payload.tgChatsIds.mkString("[", ", ", "]")}}"""
    IO(logger.info(s"Подготовка уведомления: $jsonBody")) *>
      basicRequest
        .post(tgUri.addPath("updates"))
        .body(jsonBody)
        .contentType("application/json")
        .response(asStringAlways)
        .send(httpClient)
        .flatMap { resp =>
          IO(logger.info(s"Уведомление отправлено для chatIds ${payload.tgChatsIds}: ответ: ${resp.body}")) *>
            IO.pure(resp.body)
        }
  }
}

class Notifier(
    numberUpdater: NumberUpdater,
    telegramNotifier: TelegramNotifier,
    tgUri: Uri
) {

  private def updateNumber(apiUri: Uri, identifier: String, timestamp: Long): IO[String] = {
    val payload = NumberUpdatePayload(identifier, timestamp)
    numberUpdater.updateNumber(apiUri, payload)
  }

  private def notifyTelegram(identifier: String, linksData: LinksDataResponse): IO[Option[String]] = {
    linksData.data.get(identifier).flatMap(_.headOption).map { chatId =>
      val payload = TelegramNotificationPayload(
        id = chatId,
        url = identifier,
        description = "string",
        tgChatsIds = linksData.data(identifier)
      )
      telegramNotifier.notifyTelegram(tgUri, payload).map(Some(_))
    } getOrElse {
      IO(logger.info(s"Для репозитория $identifier не найдены chatId для уведомления")).as(None)
    }
  }

  def updateNumberAndNotify(
      apiUri: Uri,
      identifier: String,
      timestamp: Long,
      linksData: LinksDataResponse
  ): IO[(String, Option[String])] = {
    for {
      updateResp <- updateNumber(apiUri, identifier, timestamp)
      notifyResp <- notifyTelegram(identifier, linksData)
    } yield (updateResp, notifyResp)
  }

  private val logger: Logger = LoggerFactory.getLogger(getClass)
}

class LinksDataFetcher(httpClient: SttpBackend[IO, Any]) {
  private val logger: Logger = LoggerFactory.getLogger(getClass)
  private def buildUri(str: String): IO[Uri] =
    IO.fromEither(Uri.parse(str).left.map(err => new RuntimeException(s"Некорректный URI '$str': $err")))

  def getLinks(apiUri: Uri): IO[LinksDataResponse] = {
    val request = basicRequest
      .get(apiUri.addPath("links").addPath("all"))
      .response(asStringAlways)
    for {
      live <- httpClient.send(request)
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
            logger.info(s"Ошибка запроса: статус ${status.code}, тело ответа: $responseBody")
            LinksDataResponse(Map.empty)
        }
      }
    } yield response
  }

  def fetchCurrentNumber(apiUri: Uri, identifier: String): IO[Option[NumberResponse]] = {
    val uri = apiUri.addPath("number").addPath(identifier)
    val req = basicRequest.get(uri).response(asStringAlways)
    httpClient.send(req).flatMap { resp =>
      if (resp.code == StatusCode.Ok)
        IO.fromEither(decode[NumberResponse](resp.body)
          .leftMap(err => new Exception(s"Decode NumberResponse error: $err")))
          .map(Some(_))
      else IO.pure(None)
    }
  }
}

class GithubCommentsProcessor(
    httpClient: SttpBackend[IO, Any],
    config: ServiceConf,
    linksDataFetcher: LinksDataFetcher,
    notifier: Notifier
) {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def processGithubComments(
      apiUri: Uri,
      tgUri: Uri,
      identifier: String,
      comments: List[GithubComment],
      linksData: LinksDataResponse
  ): IO[ProcessResult] = {
    comments.lastOption match {
      case Some(comment) =>
        IO.fromEither(
          Try(Instant.parse(comment.created_at).toEpochMilli)
            .toEither.leftMap(ex => new Exception(s"Ошибка парсинга created_at для $identifier: ${ex.getMessage}"))
        ).flatMap { newTimestamp =>
          IO(logger.info(s"Repo $identifier: новый timestamp = $newTimestamp")) *>
            linksDataFetcher.fetchCurrentNumber(apiUri, identifier).flatMap { currentOpt =>
              val currentValue = currentOpt.map(_.value).getOrElse(0L)
              IO(logger.info(s"Repo $identifier: текущее сохранённое значение = $currentValue")) *> {
                if (newTimestamp != currentValue)
                  notifier.updateNumberAndNotify(apiUri, identifier, newTimestamp, linksData).map(_ =>
                    Updated(newTimestamp)
                  )
                else
                  IO(logger.info(s"Для репозитория $identifier обновление не требуется")) *>
                    IO.pure(NoUpdateNeeded(currentValue))
              }
            }
        }
      case None =>
        IO(logger.info(s"Для репозитория $identifier не найдено ни одного комментария")) *>
          IO.pure(NoComments)
    }
  }

  def processSingleLink(
      apiUri: Uri,
      tgUri: Uri,
      identifier: String,
      linksData: LinksDataResponse
  ): IO[ProcessResult] = {
    val token          = config.githubToken
    val commentsClient = CommentsClientFactory.create[IO](token, identifier, httpClient)
    commentsClient.getComments(identifier, Some(0)).flatMap {
      case GithubCommentsResponse(comments) =>
        processGithubComments(apiUri, tgUri, identifier, comments, linksData)
      case StackOverflowCommentsResponse(response) =>
        IO(logger.info(s"StackOverflow response for $identifier: $response")) *>
          IO.pure(NoComments)
    }
  }
}

class GithubJob(httpClient: SttpBackend[IO, Any], config: ServiceConf) extends Job {
  private val logger: Logger   = LoggerFactory.getLogger(getClass)
  private val linksDataFetcher = new LinksDataFetcher(httpClient)

  private def buildUri(uri: String): Uri =
    Uri.parse(uri).getOrElse(throw new RuntimeException(s"Invalid URI: $uri"))

  override def execute(context: JobExecutionContext): Unit = {
    processLinks().handleErrorWith { err =>
      IO.raiseError(new Exception(s"Ошибка выполнения GithubJob: ${err.getMessage}"))
    }.unsafeRunSync()
  }

  private def processLinks(): IO[Unit] = {
    for {
      apiPort <- IO.fromOption(Port.fromInt(config.apiPort))(new RuntimeException("api-port is missing or invalid"))
      tgPort  <- IO.fromOption(Port.fromInt(config.tgPort))(new RuntimeException("tg-port is missing or invalid"))
      apiUri <- IO.fromOption(Uri.parse(s"http://localhost:$apiPort").toOption)(new RuntimeException(
        "api-uri is missing or invalid"
      ))
      tgUri <- IO.fromOption(Uri.parse(s"http://localhost:$tgPort").toOption)(new RuntimeException(
        "tg-uri is missing or invalid"
      ))
      linksData <- linksDataFetcher.getLinks(apiUri)
      _ <- linksData.data.keys.toList.traverse_ { identifier =>
        // Инициализируем Notifier с новыми реализациями
        val notifierInstance = new Notifier(
          new DefaultNumberUpdater(httpClient),
          new DefaultTelegramNotifier(httpClient),
          tgUri
        )
        new GithubCommentsProcessor(httpClient, config, linksDataFetcher, notifierInstance)
          .processSingleLink(apiUri, tgUri, identifier, linksData)
          .void
      }
    } yield ()
  }
}

class GithubJobFactory(httpClient: SttpBackend[IO, Any], config: ServiceConf) extends JobFactory {
  override def newJob(bundle: TriggerFiredBundle, scheduler: Scheduler): Job =
    new GithubJob(httpClient, config)
}

object QuartzApp extends IOApp.Simple {
  private def createScheduler(httpClient: SttpBackend[IO, Any], config: ServiceConf): Resource[IO, Scheduler] =
    Resource.make {
      IO {
        val scheduler: Scheduler = StdSchedulerFactory.getDefaultScheduler
        scheduler.setJobFactory(new GithubJobFactory(httpClient, config))
        scheduler.start()
        scheduler
      }
    } { scheduler =>
      IO(scheduler.shutdown())
    }

  override def run: IO[Unit] = {
    val httpClientResource = HttpClientCatsBackend.resource[IO]()
    val configIO = IO.fromOption(ConfigSource.default.load[ServiceConf].toOption)(
      new RuntimeException("cfg incorrect")
    )

    (configIO, httpClientResource.use(IO.pure)).tupled.flatMap { case (config, httpClient) =>
      createScheduler(httpClient, config).use { scheduler =>
        val jobDetail = JobBuilder.newJob(classOf[GithubJob])
          .withIdentity("GithubJob", "group1")
          .build()

        val trigger = TriggerBuilder.newTrigger()
          .withSchedule(org.quartz.CronScheduleBuilder.cronSchedule("0 */1 * ? * *"))
          .build()

        IO(scheduler.scheduleJob(jobDetail, trigger)) *> IO.never
      }
    }
  }
}
