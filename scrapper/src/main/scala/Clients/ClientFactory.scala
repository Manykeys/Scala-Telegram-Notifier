package Clients

import cats.effect.kernel.Async
import cats.syntax.functor.*

/** Общий результат запроса для комментариев */
sealed trait CommentsResponse
case class GithubCommentsResponse(comments: List[GithubComment]) extends CommentsResponse
case class StackOverflowCommentsResponse(response: StackOverflowResponse) extends CommentsResponse

/** Интерфейс для клиента, который умеет получать комментарии */
trait CommentsClient[F[_]] {
  def getComments(identifier: String, number: Option[Int] = None): F[CommentsResponse]
}

class GithubCommentsClient[F[_]: Async](token: String) extends CommentsClient[F] {
  private val client = GithubClient[F](token)
  def getComments(identifier: String, number: Option[Int] = None): F[CommentsResponse] =
    client.getComments(identifier).map(comments => GithubCommentsResponse(comments))
}

class StackoverflowCommentsClient[F[_]: Async](token: String) extends CommentsClient[F] {
  private val client = StackoverflowClient[F](token)
  def getComments(identifier: String, number: Option[Int] = None): F[CommentsResponse] = {
    val fromDate = number.getOrElse(0)
    client.getComments(identifier, fromDate).map(resp => StackOverflowCommentsResponse(resp))
  }
}

object CommentsClientFactory {
  def create[F[_]: Async](token: String, identifier: String): CommentsClient[F] =
    if (identifier.contains("/"))
      new GithubCommentsClient[F](token)
    else
      new StackoverflowCommentsClient[F](token)
}
