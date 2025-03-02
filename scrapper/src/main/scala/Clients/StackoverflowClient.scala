package Clients

import cats.effect.kernel.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.circe.generic.auto.*
import io.circe.parser.decode
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import sttp.client3.{asStringAlways, basicRequest}
import sttp.model.Uri

case class StackoverflowClient[F[_]: Async](token: String) {

  def getComments(question: String, number: Int): F[StackOverflowResponse] =
    HttpClientCatsBackend.resource[F]().use { backend =>
      val url =
        s"https://api.stackexchange.com/2.3/questions/$question/answers?fromdate=$number" +
          s"&order=desc&sort=activity&site=stackoverflow"

      val request = basicRequest
        .get(Uri.parse(url).getOrElse(throw new Exception(s"Invalid URL: $url")))
        .response(asStringAlways)

      for {
        response <- backend.send(request)
        body = response.body
        _ <- Async[F].delay(println(body))
        parsedResponse <- Async[F].fromEither(decode[StackOverflowResponse](body))
      } yield parsedResponse
    }
}
