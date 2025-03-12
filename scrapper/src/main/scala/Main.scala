package scrapper

import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s.{Host, Port}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.slf4j.{Logger, LoggerFactory}
import pureconfig.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

object Main extends IOApp:
  val logger: Logger = LoggerFactory.getLogger(getClass)
  override def run(args: List[String]): IO[ExitCode] =
    for {
      config <- IO.fromOption(ConfigSource.default.load[ServiceConf].toOption)(new RuntimeException("cfg error"))
      port   <- IO.fromOption(Port.fromInt(config.apiPort))(new Error("port parse error"))
      githubToken = config.githubToken

      endpoints <- Endpoints.all
      routes = Http4sServerInterpreter[IO]().toRoutes(endpoints)
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(Host.fromString("localhost").get)
        .withPort(port)
        .withHttpApp(Router("/" -> routes).orNotFound)
        .build
        .use: server =>
          for {
            _ <- IO(logger.info(
              s"Go to http://localhost:${server.address.getPort}/docs to open SwaggerUI. Press ENTER key to exit."
            ))
            _ <- IO.readLine
          } yield ()
    } yield ExitCode.Success
