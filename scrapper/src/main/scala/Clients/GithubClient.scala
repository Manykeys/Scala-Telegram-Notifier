package Clients

import cats.effect.kernel.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import sttp.client3.{asStringAlways, basicRequest}
import sttp.model.{StatusCode, Uri}

case class GithubClient[F[_]: Async](token: String) {

  def getComments(repo: String): F[StatusCode] =
    HttpClientCatsBackend.resource[F]().use { backend =>
      val url = Uri.unsafeParse(s"https://api.github.com/repos/$repo/pulls/comments")
      val request = basicRequest
        .get(url)
        .header("Accept", "application/vnd.github+json")
        .header("Authorization", s"Bearer $token")
        .header("X-GitHub-Api-Version", "2022-11-28")
        .response(asStringAlways)

      for {
        response <- backend.send(request)
        _        <- Async[F].delay(println(response.body))
        _        <- Async[F].delay(println(url))
      } yield response.code
    }
}
