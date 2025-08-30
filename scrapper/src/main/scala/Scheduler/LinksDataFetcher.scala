package QuartzScheduler

import Clients.{CommentsClientFactory, GithubComment, GithubCommentsResponse, StackOverflowCommentsResponse}
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.Port
import io.circe.generic.auto.*
import io.circe.parser.decode
import org.quartz.*
import org.quartz.impl.StdSchedulerFactory
import org.quartz.spi.{JobFactory, TriggerFiredBundle}
import org.slf4j.{Logger, LoggerFactory}
import pureconfig.ConfigSource
import scrapper.*
import scrapper.Models.Requests.*
import scrapper.Models.Responses.*
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import sttp.client3.{SttpBackend, asStringAlways, basicRequest}
import sttp.model.{StatusCode, Uri}
import tethys.JsonWriterOps
import tethys.jackson.*

import java.time.Instant
import scala.util.Try

class LinksDataFetcher(httpClient: SttpBackend[IO, Any]) {
  private val logger: Logger = LoggerFactory.getLogger(getClass)
  private def buildUri(str: String): IO[Uri] =
    IO.fromEither(Uri.parse(str).left.map(err => new RuntimeException(s"Некорректный URI '$str': $err")))

  def getLinks(apiUri: Uri): IO[LinksDataResponse] = {
    val request = basicRequest
      .get(apiUri.addPath("links").addPath("all"))
      .response(asStringAlways)
    for {
      live <- httpClient.send(request)
      responseBody = live.body
      response <- IO {
        live.code match {
          case StatusCode.Ok =>
            decode[LinksDataResponse](responseBody) match {
              case Right(linksData) =>
                logger.info(s"Получены данные: ${linksData.data.keys.mkString(", ")}")
                linksData
              case Left(err) =>
                logger.info(s"Ошибка парсинга JSON: $err")
                LinksDataResponse(Map.empty)
            }
          case status =>
            logger.info(s"Ошибка запроса: статус ${status.code}, тело ответа: $responseBody")
            LinksDataResponse(Map.empty)
        }
      }
    } yield response
  }

  def fetchCurrentNumber(apiUri: Uri, identifier: String): IO[Option[NumberResponse]] = {
    val uri = apiUri.addPath("number").addPath(identifier)
    val req = basicRequest.get(uri).response(asStringAlways)
    httpClient.send(req).flatMap { resp =>
      if (resp.code == StatusCode.Ok)
        IO.fromEither(decode[NumberResponse](resp.body)
          .leftMap(err => new Exception(s"Decode NumberResponse error: $err")))
          .map(Some(_))
      else IO.pure(None)
    }
  }
}
