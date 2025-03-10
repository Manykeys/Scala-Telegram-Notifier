package bot

import cats.effect.{ExitCode, IO, IOApp, Ref}
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.client.Client
import org.http4s.client.middleware.Logger
import org.slf4j.LoggerFactory
import sttp.tapir.server.http4s.Http4sServerInterpreter
import telegramium.bots.high.{Api, BotApi}

object Application extends IOApp {

  private val logger = LoggerFactory.getLogger(getClass)

  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO].resource.use { httpClient =>
      Ref.of[IO, Map[Long, PendingTrack]](Map.empty).flatMap { ref =>
        for {
          http <- IO(Logger(logBody = false, logHeaders = false)(httpClient))
          token <- IO.fromOption(sys.env.get("TGTOKEN"))(
            new RuntimeException("Переменная окружения TGTOKEN не найдена")
          )
          port <- IO.fromOption(sys.env.get("TGPORT"))(
            new RuntimeException("Переменная окружения TGPORT не найдена")
          )
          apiPort <- IO.fromOption(sys.env.get("APIPORT").map(_.toInt))(
            new RuntimeException("Переменная окружения APIPORT не найдена")
          )

          api: Api[IO] <- IO(createBotBackend(http, token))
          client  = httpClient
          echoBot = new EchoBot[IO](ref, apiPort)(api, IO.asyncForIO, IO.parallelForIO)
          botApi  = TgBotApi(echoBot)

          _ <- IO(logger.info("Setting bot commands"))
          _ <- echoBot.setMyCommands2()

          _        <- IO(logger.info("Starting bot"))
          botFiber <- echoBot.start().start

          server = Http4sServerInterpreter[IO]().toRoutes(botApi.all)
          exitCode <- BlazeServerBuilder[IO]
            .bindHttp(port.toInt, "0.0.0.0")
            .withHttpApp(server.orNotFound)
            .resource
            .use(_ => botFiber.join)
            .as(ExitCode.Success)
        } yield exitCode
      }
    }

  /** @param token
    *   Bot API token got from Botfather
    */
  private def createBotBackend(http: Client[IO], token: String) =
    BotApi(http, baseUrl = s"https://api.telegram.org/bot$token")

}
