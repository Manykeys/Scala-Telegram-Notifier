package Clients

import cats.effect.kernel.Async
import cats.syntax.functor.*
import sttp.client3.SttpBackend

sealed trait CommentsResponse
case class GithubCommentsResponse(comments: List[GithubComment])          extends CommentsResponse
case class StackOverflowCommentsResponse(response: StackOverflowResponse) extends CommentsResponse

trait CommentsClient[F[_]] {
  def getComments(identifier: String, number: Option[Int] = None): F[CommentsResponse]
}

class GithubCommentsClient[F[_]: Async, B <: SttpBackend[F, _]](token: String, backend: B) extends CommentsClient[F] {
  private val client = GithubClient[F, B](token)

  def getComments(identifier: String, number: Option[Int] = None): F[CommentsResponse] =
    client.getComments(identifier, backend).map(comments => GithubCommentsResponse(comments))
}

class StackoverflowCommentsClient[F[_]: Async, B <: SttpBackend[F, _]](token: String, backend: B)
  extends CommentsClient[F] {
  private val client = StackoverflowClient[F, B](token)

  def getComments(identifier: String, number: Option[Int] = None): F[CommentsResponse] = {
    val fromDate = number.getOrElse(0)
    client.getComments(identifier, fromDate, backend).map(resp => StackOverflowCommentsResponse(resp))
  }
}

object CommentsClientFactory {
  def create[F[_]: Async](
      token: String,
      identifier: String,
      backend: sttp.client3.SttpBackend[F, _]
  ): CommentsClient[F] =
    if (identifier.contains("/"))
      new GithubCommentsClient(token, backend)
    else
      new StackoverflowCommentsClient(token, backend)
}
