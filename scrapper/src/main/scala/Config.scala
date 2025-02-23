package scrapper

import cats.effect.Sync
import cats.syntax.all.*
import com.comcast.ip4s.Port

case class Config(httpPort: Either[Throwable, Port], githubToken: Either[Throwable, String])

trait ConfigLoader[F[_]] {
  def load: F[Config]
}

object ConfigLoader {
  def make[F[_]: Sync]: F[ConfigLoader[F]] = Sync[F].pure(new ConfigLoader[F] {
    override def load: F[Config] = for {
      port <- Sync[F].delay {
        sys.env.get("HTTP_PORT")
          .flatMap(_.toIntOption)
          .flatMap(Port.fromInt)
          .toRight(new RuntimeException("HTTP_PORT is missing or invalid"))
      }
      githubToken <- Sync[F].delay {
        sys.env.get("GITHUB_TOKEN")
          .toRight(new RuntimeException("GITHUB_TOKEN is missing"))
      }
    } yield Config(port, githubToken)
  })
}
