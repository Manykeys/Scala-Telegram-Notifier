package bot

import cats.Parallel
import cats.effect.{Async, Ref}
import cats.implicits.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.*
import io.circe.*
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import sttp.client3.{SttpBackend, UriContext, asStringAlways, basicRequest}
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
    val command = "/start"
    val description = "Запуск бота"
  }

  case object List extends Command {
    val command = "/list"
    val description = "Список отслеживаемых ссылок"
  }

  case object Track extends Command {
    val command = "/track"
    val description = "Добавить ссылку для отслеживания (с тегами и фильтрами)"
  }

  case object Untrack extends Command {
    val command = "/untrack"
    val description = "Удалить ссылку из отслеживания"
  }

  val all: List[Command] = scala.List(Start, Command.List, Track, Untrack)
}

case class PendingTrack(link: String, tags: Option[List[String]] = None, filters: Option[List[String]] = None)

class EchoBot[F[_]](pendingRef: Ref[F, Map[Long, PendingTrack]])(implicit
    bot: Api[F],
    asyncF: Async[F],
    parallel: Parallel[F]
) extends LongPollBot[F](bot) {
  private val host = uri"http://localhost:8080"
  

  override def onMessage(msg: Message): F[Unit] = {
    val chatId = msg.chat.id
    pendingRef.get.flatMap { pendingMap =>
      (pendingMap.get(chatId), msg.text) match {
        case (Some(pending), Some(text)) =>
          processPendingTrack(chatId, pending, text)
        case (None, Some(text)) =>
          handleCommand(chatId, text)
        case _ =>
          Async[F].unit
      }
    }
  }

  private def handleCommand(chatId: Long, text: String): F[Unit] = {
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
          sendMessage(ChatIntId(chatId), s"Usage: ${Command.Track.command} <link>").exec.void
      case Some(cmd) if cmd == Command.Untrack.command =>
        if (parts.length > 1)
          untrackLink(chatId, parts(1).trim)
        else
          sendMessage(ChatIntId(chatId), s"Usage: ${Command.Untrack.command} <link>").exec.void
      case _ =>
        Async[F].unit
    }
  }



  // Метод обработки команды /track
  private def handleTrackCommand(chatId: Long, cmd: String): F[Unit] = {
    val link = cmd.stripPrefix("/track ").trim
    extractLinkValue(link) match {
      case Some(extracted) =>
        pendingRef.update(_ + (chatId -> PendingTrack(extracted))) *>
          sendMessage(ChatIntId(chatId), s"Ссылка принята: $extracted. Пришлите теги (через запятую)").exec.void
      case None =>
        sendMessage(
          ChatIntId(chatId),
          "Неверный формат ссылки. Допустимые форматы: " +
            "https://stackoverflow.com/questions/{value}/* или https://github.com/{value}/{value}/*"
        ).exec.void
    }
  }

  private def processPendingTrack(chatId: Long, pending: PendingTrack, text: String): F[Unit] = {
    pending.tags match {
      case None =>
        val tags = text.split(",").map(_.trim).toList
        pendingRef.update(_.updated(chatId, pending.copy(tags = Some(tags)))) *>
          sendMessage(ChatIntId(chatId), "Теги сохранены. Теперь пришлите фильтры (через запятую)").exec.void
      case Some(_) if pending.filters.isEmpty =>
        val filters         = text.split(",").map(_.trim).toList
        val completePending = pending.copy(filters = Some(filters))
        pendingRef.update(_ - chatId) *>
          trackLinkComplete(chatId, completePending.link, completePending.tags.get, completePending.filters.get)
      case _ =>
        Async[F].unit
    }
  }

  def setMyCommands2(): F[Unit] = {
    val commands = Command.all.map(cmd => BotCommand(command = cmd.command, description = cmd.description))
    setMyCommands(commands).exec.void
  }


  private def withBackend[T](f: SttpBackend[F, Any] => F[T]): F[T] =
    HttpClientCatsBackend.resource[F]().use(f)

  private def registerChat(chatId: Long): F[Unit] =
    withBackend { backend =>
      basicRequest.post(host.addPath("tg-chat", chatId.toString))
        .response(asStringAlways)
        .send(backend)
        .attempt
        .flatMap {
          case Right(response) =>
            handleJsonResponse(
              response.body,
              chatId,
              s"Чат зарегистрирован! $chatId",
              s"Ошибка регистрации чата. $chatId"
            )
          case Left(_) =>
            sendMessage(ChatIntId(chatId), s"Ошибка! Соединение с Scrapper API не установлено $chatId").exec.void
        }
    }

  def update(chatIds: List[Long]): F[Unit] =
    chatIds.traverse_(id => sendMessage(ChatIntId(id), "победа").exec.void)

  private def listLinks(chatId: Long): F[Unit] =
    withBackend { backend =>
      basicRequest.get(host.addPath("links"))
        .header("Tg-Chat-Id", chatId.toString)
        .response(asStringAlways)
        .send(backend)
        .attempt
        .flatMap {
          case Right(response) =>
            parser.decode[ListLinksResponse](response.body) match {
              case Right(listResponse) =>
                val urls = listResponse.links.map(_.url)
                val message =
                  if (urls.isEmpty) "Нет отслеживаемых ссылок."
                  else s"Отслеживаемые ссылки:\n${urls.mkString("\n")}"
                sendMessage(ChatIntId(chatId), message).exec.void
              case Left(error) =>
                sendMessage(ChatIntId(chatId), s"Ошибка при парсинге списка ссылок: ${error.getMessage}").exec.void
            }
          case Left(_) =>
            sendMessage(ChatIntId(chatId), "Ошибка! Соединение с Scrapper API не установлено").exec.void
        }
    }


  private def trackLinkComplete(chatId: Long, link: String, tags: List[String], filters: List[String]): F[Unit] =
    withBackend { backend =>
      val jsonBody = JsonObject(
        "link"    -> link.asJson,
        "tags"    -> tags.asJson,
        "filters" -> filters.asJson
      )
      basicRequest.post(host.addPath("links"))
        .header("Tg-Chat-Id", chatId.toString)
        .body(jsonBody.asJson.noSpaces)
        .response(asStringAlways)
        .send(backend)
        .attempt
        .flatMap {
          case Right(response) =>
            handleJsonResponse(response.body, chatId, s"Ссылка добавлена: $link", "Ошибка при добавлении ссылки.")
          case Left(_) =>
            sendMessage(ChatIntId(chatId), "Ошибка! Соединение с Scrapper API не установлено").exec.void
        }
    }

  private def untrackLink(chatId: Long, link: String): F[Unit] =
    withBackend { backend =>
      val jsonBody = Json.obj("link" -> link.asJson)
      basicRequest.delete(host.addPath("links"))
        .header("Tg-Chat-Id", chatId.toString)
        .body(jsonBody.noSpaces)
        .response(asStringAlways)
        .send(backend)
        .attempt
        .flatMap {
          case Right(response) =>
            handleJsonResponse(response.body, chatId, s"Ссылка удалена: $link", "Ошибка при удалении ссылки.")
          case Left(_) =>
            sendMessage(ChatIntId(chatId), "Ошибка! Соединение с Scrapper API не установлено").exec.void
        }
    }

  private def handleJsonResponse(response: String, chatId: Long, successMsg: String, errorMsg: String): F[Unit] =
    parser.parse(response) match {
      case Right(json) if json.hcursor.downField("code").as[Int].toOption.isDefined =>
        sendMessage(ChatIntId(chatId), errorMsg).exec.void
      case Right(_) =>
        sendMessage(ChatIntId(chatId), successMsg).exec.void
      case Left(_) =>
        sendMessage(ChatIntId(chatId), successMsg).exec.void
    }

  // Функция извлечения нужного значения из ссылки
  private def extractLinkValue(link: String): Option[String] = {
    // Для StackOverflow: извлекаем значение после /questions/
    val soRegex = "https://stackoverflow\\.com/questions/([^/]+)(/.*)?".r
    // Для GitHub: извлекаем два сегмента после домена
    val ghRegex = "https://github\\.com/([^/]+)/([^/]+)(/.*)?".r
    link match {
      case soRegex(value, _)       => Some(value)
      case ghRegex(owner, repo, _) => Some(s"$owner/$repo")
      case _                       => None
    }
  }
}
