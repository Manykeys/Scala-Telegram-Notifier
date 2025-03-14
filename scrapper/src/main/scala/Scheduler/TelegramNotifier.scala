package QuartzScheduler

import cats.effect.IO
import io.circe.generic.auto.*
import org.slf4j.{Logger, LoggerFactory}
import sttp.client3.{SttpBackend, asStringAlways, basicRequest}
import sttp.model.Uri

trait TelegramNotifier {
  def notifyTelegram(tgUri: Uri, payload: TelegramNotificationPayload): IO[String]
}

case class TelegramNotificationPayload(id: Long, url: String, description: String, tgChatsIds: List[Long])

class DefaultTelegramNotifier(httpClient: SttpBackend[IO, Any]) extends TelegramNotifier {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def notifyTelegram(tgUri: Uri, payload: TelegramNotificationPayload): IO[String] = {
    val jsonBody =
      s"""{"id": ${payload.id}, "url": "${payload.url}", "description": "${payload
          .description}", "tgChatsIds": ${payload.tgChatsIds.mkString("[", ", ", "]")}}"""
    IO(logger.info(s"Подготовка уведомления: $jsonBody")) *>
      basicRequest
        .post(tgUri.addPath("updates"))
        .body(jsonBody)
        .contentType("application/json")
        .response(asStringAlways)
        .send(httpClient)
        .flatMap { resp =>
          IO(logger.info(s"Уведомление отправлено для chatIds ${payload.tgChatsIds}: ответ: ${resp.body}")) *>
            IO.pure(resp.body)
        }
  }
}
