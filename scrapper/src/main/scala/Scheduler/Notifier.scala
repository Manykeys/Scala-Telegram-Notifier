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

class Notifier(
    numberUpdater: NumberUpdater,
    telegramNotifier: TelegramNotifier,
    tgUri: Uri
) {

  private def updateNumber(apiUri: Uri, identifier: String, timestamp: Long): IO[String] = {
    val payload = NumberUpdatePayload(identifier, timestamp)
    numberUpdater.updateNumber(apiUri, payload)
  }

  private def notifyTelegram(identifier: String, linksData: LinksDataResponse): IO[Option[String]] = {
    linksData.data.get(identifier).flatMap(_.headOption).map { chatId =>
      val payload = TelegramNotificationPayload(
        id = chatId,
        url = identifier,
        description = "string",
        tgChatsIds = linksData.data(identifier)
      )
      telegramNotifier.notifyTelegram(tgUri, payload).map(Some(_))
    } getOrElse {
      IO(logger.info(s"Для репозитория $identifier не найдены chatId для уведомления")).as(None)
    }
  }

  def updateNumberAndNotify(
      apiUri: Uri,
      identifier: String,
      timestamp: Long,
      linksData: LinksDataResponse
  ): IO[(String, Option[String])] = {
    for {
      updateResp <- updateNumber(apiUri, identifier, timestamp)
      notifyResp <- notifyTelegram(identifier, linksData)
    } yield (updateResp, notifyResp)
  }

  private val logger: Logger = LoggerFactory.getLogger(getClass)
}
