package Clients

import cats.effect.kernel.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import sttp.client3.{asStringAlways, basicRequest}
import sttp.model.{StatusCode, Uri}

case class StackoverflowClient[F[_]: Async](token: String) {

  def getComments(question: String, number: Int): F[StatusCode] =
    HttpClientCatsBackend.resource[F]().use { backend =>
      Uri.parse(s"https://api.stackexchange.com/2.3/questions/" +
        s"$question/answers?fromdate=$number&order=desc&sort=activity&site=stackoverflow") match {
        case Right(url) =>
          val request = basicRequest
            .get(url)
            .response(asStringAlways)

          for {
            response <- backend.send(request)
            _        <- Async[F].delay(println(response.body))
            _        <- Async[F].delay(println(url))
          } yield response.code
        case Left(error) =>
          Async[F].raiseError(new Exception(s"Invalid URI: $error"))
      }
    }
}
