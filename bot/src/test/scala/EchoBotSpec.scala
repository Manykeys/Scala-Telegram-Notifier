package Clients

import bot.{EchoBot, PendingTrack}
import cats.effect.{IO, Ref}
import io.circe.parser
import munit.CatsEffectSuite
import sttp.client3.*
import sttp.client3.impl.cats.CatsMonadError
import sttp.client3.testing.SttpBackendStub
import sttp.model.Method

object TestHttpClientHelpers {
  def hasChatHeader(req: Request[_, _], chatId: Long): Boolean =
    req.headers.exists(h => h.name == "Tg-Chat-Id" && h.value == chatId.toString)

  def extractStringBody(req: Request[_, _]): Option[String] =
    req.body match {
      case StringBody(body, _, _) => Some(body)
      case _                      => None
    }
}

implicit val dummyApi: telegramium.bots.high.Api[IO] = new telegramium.bots.high.Api[IO] {
  override def execute[Res](method: telegramium.bots.client.Method[Res]): IO[Res] =
    IO.raiseError(new Exception("Not implemented"))
}

class TestHttpEchoBot(pendingRef: Ref[IO, Map[Long, PendingTrack]], stub: SttpBackendStub[IO, Any], apiPort: Int)
  extends EchoBot[IO](pendingRef, apiPort) {
  override def withBackend[T](f: SttpBackend[IO, Any] => IO[T]): IO[T] = f(stub)
}

class EchoBotSpec extends CatsEffectSuite {
  import TestHttpClientHelpers.*

  private val hostBase = "http://localhost:8080"

  test("registerChat отправляет корректный POST запрос") {
    val chatId      = 1L
    val expectedUrl = s"$hostBase/tg-chat/$chatId"

    val monad = new CatsMonadError[IO]
    val stub = SttpBackendStub[IO, Any](monad)
      .whenRequestMatchesPartial {
        case req
            if req.uri.toString == expectedUrl &&
              req.method == Method.POST &&
              hasChatHeader(req, chatId) &&
              extractStringBody(req).contains("") =>
          Response.ok("{}")
      }

    for {
      ref <- Ref.of[IO, Map[Long, PendingTrack]](Map.empty)
      bot = new TestHttpEchoBot(ref, stub, 8080)
      result <- bot.registerChat(chatId)
    } yield assertEquals(result, "Чат зарегистрирован!")
  }

  test("listLinks возвращает корректное сообщение для непустого списка") {
    val chatId      = 1L
    val expectedUrl = s"$hostBase/links"
    val jsonResponse =
      """{
        |  "links": [
        |    {
        |      "id": 1,
        |      "url": "https://github.com/owner/repo",
        |      "tags": ["tag1", "tag2"],
        |      "filters": ["filter1", "filter2"]
        |    }
        |  ],
        |  "size": 1
        |}""".stripMargin

    val monad = new CatsMonadError[IO]
    val stub = SttpBackendStub[IO, Any](monad)
      .whenRequestMatchesPartial {
        case req
            if req.uri.toString == expectedUrl &&
              req.method == Method.GET &&
              hasChatHeader(req, chatId) =>
          Response.ok(jsonResponse)
      }

    for {
      ref <- Ref.of[IO, Map[Long, PendingTrack]](Map.empty)
      bot = new TestHttpEchoBot(ref, stub, 8080)
      result <- bot.listLinks(chatId)
    } yield assertEquals(result, "Отслеживаемые ссылки:\nhttps://github.com/owner/repo")
  }

  test("trackLinkComplete отправляет корректный POST запрос с JSON телом") {
    val chatId      = 1L
    val link        = "owner/repo"
    val tags        = List("tag1", "tag2")
    val filters     = List("filter1", "filter2")
    val expectedUrl = s"$hostBase/links"

    val monad = new CatsMonadError[IO]
    val stub = SttpBackendStub[IO, Any](monad)
      .whenRequestMatchesPartial {
        case req
            if req.uri.toString == expectedUrl &&
              req.method == Method.POST &&
              hasChatHeader(req, chatId) &&
              extractStringBody(req).exists { body =>
                parser.parse(body).toOption.exists { json =>
                  val cursor = json.hcursor
                  cursor.get[String]("link").toOption.contains(link) &&
                  cursor.get[List[String]]("tags").toOption.exists(ts => ts.sorted == tags.sorted) &&
                  cursor.get[List[String]]("filters").toOption.exists(fs => fs.sorted == filters.sorted)
                }
              } =>
          Response.ok("{}")
      }

    for {
      ref <- Ref.of[IO, Map[Long, PendingTrack]](Map.empty)
      bot = new TestHttpEchoBot(ref, stub, 8080)
      result <- bot.trackLinkComplete(chatId, link, Option(tags), Option(filters))
    } yield assertEquals(result, s"Ссылка добавлена: $link")
  }

  test("untrackLink отправляет корректный DELETE запрос с JSON телом") {
    val chatId      = 1L
    val link        = "https://github.com/owner/repo"
    val expectedUrl = s"$hostBase/links"

    val monad = new CatsMonadError[IO]
    val stub = SttpBackendStub[IO, Any](monad)
      .whenRequestMatchesPartial {
        case req
            if req.uri.toString == expectedUrl &&
              req.method == Method.DELETE &&
              hasChatHeader(req, chatId) &&
              extractStringBody(req).exists { body =>
                parser.parse(body).toOption.exists { json =>
                  json.hcursor.get[String]("link").toOption.contains(link)
                }
              } =>
          Response.ok("{}")
      }

    for {
      ref <- Ref.of[IO, Map[Long, PendingTrack]](Map.empty)
      bot = new TestHttpEchoBot(ref, stub, 8080)
      result <- bot.untrackLink(chatId, link)
    } yield assertEquals(result, s"Ссылка удалена: $link")
  }

  test("неизвестная команда выдает ошибку") {
    val chatId         = 1L
    val unknownCommand = "/unknown"

    val monad = new CatsMonadError[IO]
    val stub  = SttpBackendStub[IO, Any](monad)
    for {
      ref <- Ref.of[IO, Map[Long, PendingTrack]](Map.empty)
      bot = new TestHttpEchoBot(ref, stub, 8080)
      result <- bot.handleCommand(chatId, unknownCommand)
    } yield assertEquals(result, "Ошибка: неизвестная команда")
  }
}
