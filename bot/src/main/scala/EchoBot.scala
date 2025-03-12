package bot

import cats.Parallel
import cats.effect.{Async, Ref}
import cats.implicits.*
import io.circe.*
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.*
import org.slf4j.{Logger, LoggerFactory}
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import sttp.client3.{SttpBackend, asStringAlways, basicRequest}
import sttp.model.Uri
import telegramium.bots.high.implicits.methodOps
import telegramium.bots.high.{Api, LongPollBot}
import telegramium.bots.{BotCommand, ChatIntId, Message}

case class LinkResponse(id: Long, url: String, tags: List[String], filters: List[String])

object LinkResponse {
  implicit val decoder: Decoder[LinkResponse] = deriveDecoder
  implicit val encoder: Encoder[LinkResponse] = deriveEncoder
}

case class ListLinksResponse(links: List[LinkResponse], size: Int)

object ListLinksResponse {
  implicit val decoder: Decoder[ListLinksResponse] = deriveDecoder
  implicit val encoder: Encoder[ListLinksResponse] = deriveEncoder
}

sealed trait Command {
  def command: String
  def description: String
}

object Command {
  case object Start extends Command {
    val command     = "/start"
    val description = "Запуск бота"
  }

  case object List extends Command {
    val command     = "/list"
    val description = "Список отслеживаемых ссылок"
  }

  case object Track extends Command {
    val command     = "/track"
    val description = "Добавить ссылку для отслеживания (с тегами и фильтрами)"
  }

  case object Untrack extends Command {
    val command     = "/untrack"
    val description = "Удалить ссылку из отслеживания"
  }

  val all: List[Command] = scala.List(Start, Command.List, Track, Untrack)
}

case class PendingTrack(link: String, tags: Option[List[String]] = None, filters: Option[List[String]] = None)

case class EchoBot[F[_]](pendingRef: Ref[F, Map[Long, PendingTrack]], apiPort: Int)(implicit
    bot: Api[F],
    asyncF: Async[F],
    parallel: Parallel[F]
) extends LongPollBot[F](bot) {

  private val apiLink: String = s"http://localhost:$apiPort"

  def getHost[F[_]: Async]: F[Uri] = {
    Async[F].fromEither(Uri.parse(apiLink).leftMap(e => new Throwable(s"Invalid URL: $apiLink")))
  }

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def onMessage(msg: Message): F[Unit] = {
    val chatId = msg.chat.id
    pendingRef.get.flatMap { pendingMap =>
      (pendingMap.get(chatId), msg.text) match {
        case (Some(pending), Some(text)) =>
          processPendingTrack(chatId, pending, text).flatMap(sendMessage(chatId, _))
        case (None, Some(text)) =>
          handleCommand(chatId, text).flatMap(sendMessage(chatId, _))
        case _ =>
          Async[F].unit
      }
    }
  }

  def handleCommand(chatId: Long, text: String): F[String] = {
    val parts = text.split(" ", 2)
    parts.headOption match {
      case Some(cmd) if cmd == Command.Start.command =>
        registerChat(chatId)
      case Some(cmd) if cmd == Command.List.command =>
        listLinks(chatId)
      case Some(cmd) if cmd == Command.Track.command =>
        if (parts.length > 1)
          handleTrackCommand(chatId, text)
        else
          Async[F].pure(s"Usage: ${Command.Track.command} <link>")
      case Some(cmd) if cmd == Command.Untrack.command =>
        if (parts.length > 1)
          untrackLink(chatId, parts(1).trim)
        else
          Async[F].pure(s"Usage: ${Command.Untrack.command} <link>")
      case _ =>
        Async[F].pure("Ошибка: неизвестная команда")
    }
  }

  def handleTrackCommand(chatId: Long, cmd: String): F[String] = {
    val link = cmd.stripPrefix("/track ").trim
    extractLinkValue(link) match {
      case Some(extracted) =>
        pendingRef.update(_ + (chatId -> PendingTrack(extracted))) *>
          Async[F].pure(
            s"Ссылка принята: $extracted. Пришлите теги (через запятую) или напишите 'skip' для пропуска."
          )
      case None =>
        Async[F].pure(
          "Неверный формат ссылки. Допустимые форматы: https://stackoverflow.com/questions/{value}/* или https://github.com/{value}/{value}/*"
        )
    }
  }

  def processPendingTrack(chatId: Long, pending: PendingTrack, text: String): F[String] = {
    pending.tags match {
      case None =>
        if (text.trim.toLowerCase == "skip") {
          pendingRef.update(_.updated(chatId, pending.copy(tags = Some(List.empty)))) *>
            Async[F].pure(
              "Теги пропущены. Теперь пришлите фильтры (через запятую) или напишите 'skip' для пропуска."
            )
        } else {
          val tags = text.split(",").map(_.trim).filter(_.nonEmpty).toList
          pendingRef.update(_.updated(chatId, pending.copy(tags = Some(tags)))) *>
            Async[F].pure(
              "Теги сохранены. Теперь пришлите фильтры (через запятую) или напишите 'skip' для пропуска."
            )
        }
      case Some(_) if pending.filters.isEmpty =>
        if (text.trim.toLowerCase == "skip") {
          val completePending = pending.copy(filters = Some(List.empty))
          pendingRef.update(_ - chatId) *>
            trackLinkComplete(chatId, completePending.link, completePending.tags, completePending.filters)
        } else {
          val filters         = text.split(",").map(_.trim).filter(_.nonEmpty).toList
          val completePending = pending.copy(filters = Some(filters))
          pendingRef.update(_ - chatId) *>
            trackLinkComplete(chatId, completePending.link, completePending.tags, completePending.filters)
        }
      case _ =>
        Async[F].pure("")
    }
  }

  def setMyCustomCommands(): F[Unit] = {
    val commands = Command.all.map(cmd => BotCommand(command = cmd.command, description = cmd.description))
    setMyCommands(commands).exec.void
  }

  def withBackend[T](f: SttpBackend[F, Any] => F[T]): F[T] =
    HttpClientCatsBackend.resource[F]().use(f)

  def registerChat(chatId: Long): F[String] = {
    logger.info(s"Регистрация чата: $chatId")
    getHost.flatMap(host =>
      apiPost(host.addPath("tg-chat", chatId.toString), chatId, "Чат зарегистрирован!", "Ошибка регистрации чата.")
    )
  }

  def update(chatIds: List[Long]): F[Unit] =
    chatIds.traverse_(id => sendMessage(id, "победа"))

  def listLinks(chatId: Long): F[String] =
    getHost.flatMap(host =>
      getDecodedResponse[ListLinksResponse](host.addPath("links"), chatId).flatMap {
        case Right(listResponse) =>
          val urls = listResponse.links.map(_.url)
          val message =
            if (urls.isEmpty) "Нет отслеживаемых ссылок."
            else s"Отслеживаемые ссылки:\n${urls.mkString("\n")}"
          Async[F].pure(message)
        case Left(error) =>
          Async[F].pure(s"Ошибка при получении списка ссылок: $error")
      }
    )

  def trackLinkComplete(
      chatId: Long,
      link: String,
      tags: Option[List[String]],
      filters: Option[List[String]]
  ): F[String] =
    getHost.flatMap(host =>
      apiPost(
        host.addPath("links"),
        chatId,
        s"Ссылка добавлена: $link",
        "Ошибка при добавлении ссылки.",
        Some(Json.obj("link" -> link.asJson, "tags" -> tags.asJson, "filters" -> filters.asJson).noSpaces)
      )
    )

  def untrackLink(chatId: Long, link: String): F[String] =
    getHost.flatMap(host =>
      apiDelete(
        host.addPath("links"),
        chatId,
        s"Ссылка удалена: $link",
        "Ошибка при удалении ссылки.",
        Some(Json.obj("link" -> link.asJson).noSpaces)
      )
    )

  def apiGet(uri: Uri, chatId: Long, successMsg: String, errorMsg: String): F[String] =
    getHost.flatMap(host =>
      apiRequest(basicRequest.get(uri).header("Tg-Chat-Id", chatId.toString), chatId, successMsg, errorMsg)
    )

  def apiPost(uri: Uri, chatId: Long, successMsg: String, errorMsg: String, body: Option[String] = None): F[String] =
    getHost.flatMap(host =>
      apiRequest(
        basicRequest.post(uri).header("Tg-Chat-Id", chatId.toString).body(body.getOrElse("")),
        chatId,
        successMsg,
        errorMsg
      )
    )

  def apiDelete(uri: Uri, chatId: Long, successMsg: String, errorMsg: String, body: Option[String] = None): F[String] =
    getHost.flatMap(host =>
      apiRequest(
        basicRequest.delete(uri).header("Tg-Chat-Id", chatId.toString).body(body.getOrElse("")),
        chatId,
        successMsg,
        errorMsg
      )
    )

  def apiRequest(
      request: sttp.client3.RequestT[sttp.client3.Identity, Either[String, String], Any],
      chatId: Long,
      successMsg: String,
      errorMsg: String
  ): F[String] =
    withBackend { backend =>
      request.response(asStringAlways)
        .send(backend)
        .attempt
        .flatMap {
          case Right(response) =>
            handleJsonResponse(response.body, chatId, successMsg, errorMsg)
          case Left(_) =>
            Async[F].pure("Ошибка! Соединение с Scrapper API не установлено")
        }
    }

  def handleJsonResponse(response: String, chatId: Long, successMsg: String, errorMsg: String): F[String] =
    parser.parse(response) match {
      case Right(json) if json.hcursor.downField("code").as[Int].toOption.isDefined =>
        Async[F].pure(errorMsg)
      case Right(_) =>
        Async[F].pure(successMsg)
      case Left(_) =>
        Async[F].pure(successMsg)
    }

  def extractLinkValue(link: String): Option[String] = {
    val soRegex = "https://stackoverflow\\.com/questions/([^/]+)(/.*)?".r
    val ghRegex = "https://github\\.com/([^/]+)/([^/]+)(/.*)?".r
    link match {
      case soRegex(value, _)       => Some(value)
      case ghRegex(owner, repo, _) => Some(s"$owner/$repo")
      case _                       => None
    }
  }

  def getDecodedResponse[A: Decoder](uri: Uri, chatId: Long): F[Either[String, A]] =
    withBackend { backend =>
      basicRequest
        .get(uri)
        .header("Tg-Chat-Id", chatId.toString)
        .response(asStringAlways)
        .send(backend)
        .attempt
    }.map {
      case Right(response) =>
        parser.decode[A](response.body).left.map(_.getMessage)
      case Left(_) =>
        Left("Ошибка! Соединение с Scrapper API не установлено")
    }

  def sendMessage(chatId: Long, message: String): F[Unit] = {
    logger.info(s"Отправка сообщения в чат $chatId: $message")
    sendMessage(ChatIntId(chatId), message).exec.void
  }
}
