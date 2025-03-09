package Clients

import cats.effect.kernel.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.circe.generic.auto.*
import io.circe.parser.decode
import sttp.client3.{SttpBackend, asStringAlways, basicRequest}
import sttp.model.Uri

case class GithubClient[F[_]: Async, B <: SttpBackend[F, _]](token: String) {

  def getComments(repo: String, backend: B): F[List[GithubComment]] =
    getAllComments(backend, repo, 1, None)

  def getAllComments(
                      backend: B,
                      repo: String,
                      page: Int,
                      previousComments: Option[List[GithubComment]]
                    ): F[List[GithubComment]] = {
    val url = s"https://api.github.com/repos/$repo/pulls/comments?per_page=100&page=$page"

    val request = basicRequest
      .get(Uri.parse(url).getOrElse(throw new Exception(s"Invalid URL: $url")))
      .header("Accept", "application/vnd.github+json")
      .header("Authorization", s"Bearer $token")
      .header("X-GitHub-Api-Version", "2022-11-28")
      .response(asStringAlways)

    for {
      response <- backend.send(request)
      _        <- Async[F].delay(println(s"URL: $url"))
      _        <- Async[F].delay(println(s"Response JSON: ${response.body}"))
      comments <- Async[F].fromEither(decode[List[GithubComment]](response.body))
      result <- if (comments.nonEmpty) {
        getAllComments(backend, repo, page + 1, Some(comments))
      } else {
        previousComments match {
          case Some(previous) => Async[F].pure(previous)
          case None           => Async[F].pure(List.empty)
        }
      }
    } yield result
  }
}
