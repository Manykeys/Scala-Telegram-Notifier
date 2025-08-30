package QuartzScheduler

import cats.effect.IO
import org.slf4j.{Logger, LoggerFactory}
import scrapper.*
import scrapper.Models.Requests.*
import sttp.client3.{SttpBackend, asStringAlways, basicRequest}
import sttp.model.Uri
import tethys.JsonWriterOps
import tethys.jackson.*

trait NumberUpdater {
  def updateNumber(apiUri: Uri, payload: NumberUpdatePayload): IO[String]
}

case class NumberUpdatePayload(identifier: String, timestamp: Long)

class DefaultNumberUpdater(httpClient: SttpBackend[IO, Any]) extends NumberUpdater {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def buildPayload(identifier: String, timestamp: Long): NumberUpdatePayload =
    NumberUpdatePayload(identifier, timestamp)

  override def updateNumber(apiUri: Uri, payload: NumberUpdatePayload): IO[String] = {
    val req = basicRequest
      .post(apiUri.addPath("number"))
      .body(AddNumberRequest(payload.identifier, payload.timestamp).asJson)
      .response(asStringAlways)
    httpClient.send(req).flatMap { resp =>
      IO(logger.info(s"Обновление числа для ${payload.identifier} получило ответ: ${resp.body}")) *>
        IO.pure(resp.body)
    }
  }
}
