package scrapper

import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s.{Host, Port}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import sttp.tapir.server.http4s.Http4sServerInterpreter

object Main extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    for {
      configLoader <- ConfigLoader.make[IO]
      config       <- configLoader.load
      maybePort        = config.httpPort.toOption
      maybeGithubToken = config.githubToken.toOption

      endpoints <- Endpoints.all
      routes = Http4sServerInterpreter[IO]().toRoutes(endpoints)

      port <- maybePort match {
        case Some(p) => IO.pure(p)
        case None    => IO.raiseError(new RuntimeException("HTTP_PORT is missing or invalid"))
      }

      githubToken <- maybeGithubToken match {
        case Some(p) => IO.pure(p)
        case None    => IO.raiseError(new RuntimeException("GITHUB_TOKEN is missing or invalid"))
      }

      _ <- EmberServerBuilder
        .default[IO]
        .withHost(Host.fromString("localhost").get)
        .withPort(port)
        .withHttpApp(Router("/" -> routes).orNotFound)
        .build
        .use: server =>
          for
            _ <- IO.println(
              s"Go to http://localhost:${server.address.getPort}/docs to open SwaggerUI. Press ENTER key to exit."
            )
            _ <- IO.readLine
          yield ()
    } yield ExitCode.Success
