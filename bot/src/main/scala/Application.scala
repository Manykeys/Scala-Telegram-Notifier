package bot

import cats.effect.{ExitCode, IO, IOApp, Ref}
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.client.Client
import org.http4s.client.middleware.Logger
import sttp.tapir.server.http4s.Http4sServerInterpreter
import telegramium.bots.high.{Api, BotApi}

object Application extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO].resource.use { httpClient =>
      val http                           = Logger(logBody = false, logHeaders = false)(httpClient)
      val token                          = sys.env("TGTOKEN")
      implicit val api: Api[IO]          = createBotBackend(http, token)
      implicit val httpClien: Client[IO] = httpClient
      Ref.of[IO, Map[Long, PendingTrack]](Map.empty).flatMap {
        ref =>
          val echoBot = new EchoBot[IO](ref)
          val botApi  = TgBotApi(echoBot)

          for {
            _        <- echoBot.setMyCommands2()
            botFiber <- echoBot.start().start
            server = Http4sServerInterpreter[IO]().toRoutes(botApi.all)
            exitCode <- BlazeServerBuilder[IO]
              .bindHttp(sys.env("PORT").toInt, "0.0.0.0")
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
