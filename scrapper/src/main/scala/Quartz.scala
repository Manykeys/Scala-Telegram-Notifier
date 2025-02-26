import cats.effect.{IO, IOApp, Resource}
import cats.effect.unsafe.implicits.global
import org.quartz.*
import org.quartz.impl.StdSchedulerFactory
import io.circe.generic.auto.*
import io.circe.parser.decode
import cats.syntax.all.*
import sttp.model.StatusCode
import Clients.GithubClient
import org.http4s.blaze.client.BlazeClientBuilder
import scrapper.Endpoints.LinksDataResponse
import sttp.client3.{UriContext, asStringAlways, emptyRequest}
import sttp.client3.httpclient.cats.HttpClientCatsBackend

class GithubJob extends Job {
  override def execute(context: JobExecutionContext): Unit = {
    val getToken: IO[String] = IO.fromOption(sys.env.get("GITHUB_TOKEN"))(
      new RuntimeException("Переменная окружения GITHUB_TOKEN не найдена")
    )

    val clientResource = BlazeClientBuilder[IO].resource

    def getLinks: IO[LinksDataResponse] = {
      HttpClientCatsBackend.resource[IO]().use { backend =>
        val request = emptyRequest.get(uri"http://localhost:8080/links/all").response(asStringAlways)

        for {
          live <- backend.send(request)
          responseBody = live.body
          responseStatus = live.code
          response <- IO {
            responseStatus match {
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

    val processLinks: IO[Unit] = for {
      token <- getToken
      githubClient = GithubClient[IO](token)
      linksData <- getLinks
      _ <- linksData.data.keys.toList.traverse_ { repo =>
        for {
          status <- githubClient.getComments(repo)
          _ <- IO.println(s"GitHub API response for $repo: $status")
        } yield ()
      }
    } yield ()

    processLinks.handleErrorWith { err =>
      IO.println(s"Ошибка выполнения GitHubJob: ${err.getMessage}")
    }.unsafeRunSync()
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
      val jobDetail = org.quartz.JobBuilder.newJob(classOf[GithubJob])
        .withIdentity("GithubJob", "group1")
        .build()

      val trigger = TriggerBuilder.newTrigger()
        .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *"))
        .build()

      IO(scheduler.scheduleJob(jobDetail, trigger)) *> IO.never
    }
  }
}