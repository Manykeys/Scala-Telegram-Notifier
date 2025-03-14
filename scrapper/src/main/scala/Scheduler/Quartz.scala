package QuartzScheduler

import Clients.{CommentsClientFactory, GithubComment, GithubCommentsResponse, StackOverflowCommentsResponse}
import QuartzScheduler.Models.*
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.Port
import org.quartz.*
import org.quartz.impl.StdSchedulerFactory
import org.quartz.spi.{JobFactory, TriggerFiredBundle}
import org.slf4j.{Logger, LoggerFactory}
import pureconfig.ConfigSource
import scrapper.*
import scrapper.Models.Responses.*
import sttp.client3.SttpBackend
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import sttp.model.Uri

import java.time.Instant
import scala.util.Try

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
