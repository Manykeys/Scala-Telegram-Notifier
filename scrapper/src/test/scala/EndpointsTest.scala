package scrapper

import cats.effect.IO
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.headers.`Content-Type`
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import sttp.tapir.server.http4s.Http4sServerInterpreter

class EndpointsTest extends CatsEffectSuite {

  private val routesIO = Endpoints.all.map { endpoints =>
    Http4sServerInterpreter[IO]().toRoutes(endpoints)
  }

  test("Endpoint регистрации чата должен зарегистрировать чат") {
    routesIO.flatMap { routes =>
      val req = Request[IO](method = Method.POST, uri = uri"/tg-chat/123")
      routes.run(req).value.flatMap {
        case Some(resp) =>
          IO(assertEquals(resp.status, Status.Ok))
        case None =>
          IO(fail("Не получен ответ при регистрации чата"))
      }
    }
  }

  test("Endpoint добавления ссылки должен успешно добавить ссылку") {
    routesIO.flatMap { routes =>
      val registerReq = Request[IO](method = Method.POST, uri = uri"/tg-chat/123")
      for {
        _ <- routes.run(registerReq).value
        addLinkJson =
          """{"link": "repo/owner", "tags": ["tag1"], "filters": ["filter1"]}"""
        addLinkReq = Request[IO](method = Method.POST, uri = uri"/links")
          .withEntity(addLinkJson)
          .withHeaders(
            Header.Raw(CIString("Tg-Chat-Id"), "123"),
            `Content-Type`(MediaType.application.json)
          )
        respOpt <- routes.run(addLinkReq).value
        resp <- respOpt match {
          case Some(r) => IO.pure(r)
          case None    => IO.raiseError(new Exception("Не получен ответ при добавлении ссылки"))
        }
        body <- resp.as[String]
        _ = assert(body.contains("repo/owner"), "Ответ должен содержать добавленную ссылку")
      } yield ()
    }
  }

  test("Endpoint Дублирующаяся ссылка не должна быть добавлена") {
    routesIO.flatMap { routes =>
      val chatId      = "123"
      val registerReq = Request[IO](method = Method.POST, uri = uri"/tg-chat/123")
      val addLinkJson =
        """{"link": "repo/owner", "tags": ["tag1"], "filters": ["filter1"]}"""
      val addLinkReq = Request[IO](method = Method.POST, uri = uri"/links")
        .withEntity(addLinkJson)
        .withHeaders(
          Header.Raw(CIString("Tg-Chat-Id"), chatId),
          `Content-Type`(MediaType.application.json)
        )
      for {
        _            <- routes.run(registerReq).value
        firstRespOpt <- routes.run(addLinkReq).value
        firstResp <- firstRespOpt match {
          case Some(r) => IO.pure(r)
          case None    => IO.raiseError(new Exception("Не получен ответ при первом добавлении ссылки"))
        }
        _             <- IO(assertEquals(firstResp.status, Status.Ok))
        secondRespOpt <- routes.run(addLinkReq).value
        secondResp <- secondRespOpt match {
          case Some(r) => IO.pure(r)
          case None    => IO.raiseError(new Exception("Не получен ответ при повторном добавлении ссылки"))
        }
        _          <- IO(assertEquals(secondResp.status, Status.BadRequest))
        secondBody <- secondResp.as[String]
        _ = assert(secondBody.contains("Duplicate link"), "Сообщение об ошибке должно указывать на дублирование")
      } yield ()
    }
  }

  test("Endpoint получения ссылок должен вернуть только одну ссылку после попытки повторного добавления") {
    routesIO.flatMap { routes =>
      val chatId      = "123"
      val registerReq = Request[IO](method = Method.POST, uri = uri"/tg-chat/123")
      val addLinkJson =
        """{"link": "repo/owner", "tags": ["tag1"], "filters": ["filter1"]}"""
      val addLinkReq = Request[IO](method = Method.POST, uri = uri"/links")
        .withEntity(addLinkJson)
        .withHeaders(
          Header.Raw(CIString("Tg-Chat-Id"), chatId),
          `Content-Type`(MediaType.application.json)
        )
      val getLinksReq = Request[IO](method = Method.GET, uri = uri"/links")
        .withHeaders(Header.Raw(CIString("Tg-Chat-Id"), chatId))
      for {
        _          <- routes.run(registerReq).value
        _          <- routes.run(addLinkReq).value
        _          <- routes.run(addLinkReq).value
        getRespOpt <- routes.run(getLinksReq).value
        getResp <- getRespOpt match {
          case Some(r) => IO.pure(r)
          case None    => IO.raiseError(new Exception("Не получен ответ при получении списка ссылок"))
        }
        getBody <- getResp.as[String]
        occurrences = "repo/owner".r.findAllMatchIn(getBody).length
        _           = assertEquals(occurrences, 1, "Ссылка должна присутствовать только один раз")
      } yield ()
    }
  }
}
