package scrapper

import cats.effect.IO
import scrapper.repository.LinkRepository
import sttp.tapir.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.tethysjson.jsonBody
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import tethys.readers.tokens.TokenIterator
import tethys.writers.tokens.TokenWriter
import tethys.{JsonObjectWriter, JsonReader, JsonWriter, readers}

object Endpoints:
  case class ApiErrorResponse(
      description: String,
      code: String,
      exceptionName: Option[String],
      exceptionMessage: Option[String],
      stacktrace: Option[List[String]]
  )

  case class LinkResponse(id: Long, url: String, tags: List[String], filters: List[String])
  case class AddLinkRequest(link: String, tags: List[String], filters: List[String])
  case class RemoveLinkRequest(link: String)
  case class ListLinksResponse(links: List[LinkResponse], size: Int)
  case class LinksDataResponse(data: Map[String, List[List[Long]]])

  implicit val tupleWriter: JsonWriter[(Long, List[String], List[String])] =
    new JsonWriter[(Long, List[String], List[String])] {
      def write(value: (Long, List[String], List[String]), tokenWriter: TokenWriter): Unit = {
        tokenWriter.writeArrayStart()
        tokenWriter.writeNumber(value._1)
        tokenWriter.writeArrayStart()
        value._2.foreach(tokenWriter.writeString)
        tokenWriter.writeArrayEnd()
        tokenWriter.writeArrayStart()
        value._3.foreach(tokenWriter.writeString)
        tokenWriter.writeArrayEnd()
        tokenWriter.writeArrayEnd()
      }
    }

  implicit val apiErrorResponseReader: JsonReader[ApiErrorResponse]       = JsonReader.derived
  implicit val apiErrorResponseWriter: JsonObjectWriter[ApiErrorResponse] = JsonObjectWriter.derived

  implicit val linkResponseReader: JsonReader[LinkResponse]       = JsonReader.derived
  implicit val linkResponseWriter: JsonObjectWriter[LinkResponse] = JsonObjectWriter.derived

  implicit val addLinkRequestReader: JsonReader[AddLinkRequest]       = JsonReader.derived
  implicit val addLinkRequestWriter: JsonObjectWriter[AddLinkRequest] = JsonObjectWriter.derived

  implicit val removeLinkRequestReader: JsonReader[RemoveLinkRequest]       = JsonReader.derived
  implicit val removeLinkRequestWriter: JsonObjectWriter[RemoveLinkRequest] = JsonObjectWriter.derived

  implicit val listLinksResponseReader: JsonReader[ListLinksResponse]       = JsonReader.derived
  implicit val listLinksResponseWriter: JsonObjectWriter[ListLinksResponse] = JsonObjectWriter.derived

  implicit val linksDataResponseReader: JsonReader[LinksDataResponse]       = JsonReader.derived
  implicit val linksDataResponseWriter: JsonObjectWriter[LinksDataResponse] = JsonObjectWriter.derived

  def registerChatLogic(id: Long, repository: LinkRepository[IO]): IO[Either[ApiErrorResponse, Unit]] =
    repository.registerChat(id).map { _ =>
      println(s"Registered chat with ID $id")
      Right(())
    }

  def deleteChatLogic(id: Long, repository: LinkRepository[IO]): IO[Either[ApiErrorResponse, Unit]] =
    repository.deleteChat(id).map { _ =>
      println(s"Deleted chat with ID $id")
      Right(())
    }

  def getLinksLogic(chatId: Long, repository: LinkRepository[IO]): IO[Either[ApiErrorResponse, ListLinksResponse]] =
    repository.getLinks(chatId).map { links =>
      println(s"Fetched links for chat $chatId: ${links.size}")
      Right(ListLinksResponse(links, links.size))
    }

  def addLinkLogic(
      chatId: Long,
      request: AddLinkRequest,
      repository: LinkRepository[IO]
  ): IO[Either[ApiErrorResponse, LinkResponse]] =
    repository.addLink(chatId, request.link, request.tags, request.filters).map {
      case Some(link) =>
        println(s"Added link for chat $chatId: ${link.url}")
        Right(link)
      case None =>
        Left(ApiErrorResponse("Link not added", "404", None, None, None))
    }

  def removeLinkLogic(
      chatId: Long,
      request: RemoveLinkRequest,
      repository: LinkRepository[IO]
  ): IO[Either[ApiErrorResponse, LinkResponse]] =
    repository.removeLink(chatId, request.link).map {
      case Some(link) =>
        println(s"Removed link for chat $chatId: ${link.url}")
        Right(link)
      case None =>
        Left(ApiErrorResponse("Link not found", "404", None, None, None))
    }

  def getAllLinksLogic(repository: LinkRepository[IO]): IO[Either[ApiErrorResponse, LinksDataResponse]] =
    repository.getAllUrlData.map {
      case data if data.nonEmpty => Right(LinksDataResponse(data))
      case _                     => Left(ApiErrorResponse("No data found", "404", None, None, None))
    }

  def createLinkRepository: IO[LinkRepository[IO]] = LinkRepository.create

  val allEndpoints: IO[List[ServerEndpoint[Any, IO]]] = for {
    repository <- createLinkRepository
    registerChat = endpoint.post
      .in("tg-chat" / path[Long]("id"))
      .errorOut(jsonBody[ApiErrorResponse])
      .out(emptyOutput)
      .serverLogic { id =>
        registerChatLogic(id, repository)
      }
    deleteChat = endpoint.delete
      .in("tg-chat" / path[Long]("id"))
      .errorOut(jsonBody[ApiErrorResponse])
      .out(emptyOutput)
      .serverLogic { id =>
        deleteChatLogic(id, repository)
      }
    getLinks = endpoint.get
      .in("links")
      .in(header[Long]("Tg-Chat-Id"))
      .errorOut(jsonBody[ApiErrorResponse])
      .out(jsonBody[ListLinksResponse])
      .serverLogic { chatId =>
        getLinksLogic(chatId, repository)
      }
    addLink = endpoint.post
      .in("links")
      .in(header[Long]("Tg-Chat-Id"))
      .in(jsonBody[AddLinkRequest])
      .errorOut(jsonBody[ApiErrorResponse])
      .out(jsonBody[LinkResponse])
      .serverLogic { (chatId, request) =>
        addLinkLogic(chatId, request, repository)
      }
    removeLink = endpoint.delete
      .in("links")
      .in(header[Long]("Tg-Chat-Id"))
      .in(jsonBody[RemoveLinkRequest])
      .errorOut(jsonBody[ApiErrorResponse])
      .out(jsonBody[LinkResponse])
      .serverLogic { (chatId, request) =>
        removeLinkLogic(chatId, request, repository)
      }
    getAllLinks = endpoint.get
      .in("links" / "all")
      .errorOut(jsonBody[ApiErrorResponse])
      .out(jsonBody[LinksDataResponse])
      .serverLogic { _ =>
        getAllLinksLogic(repository)
      }
  } yield List(registerChat, deleteChat, getLinks, addLink, removeLink, getAllLinks)

  val apiEndpoints: IO[List[ServerEndpoint[Any, IO]]] = allEndpoints

  val docEndpoints: IO[List[ServerEndpoint[Any, IO]]] = apiEndpoints.flatMap { api =>
    IO(SwaggerInterpreter().fromServerEndpoints[IO](api, "Scrapper API", "1.0.0"))
  }

  val all: IO[List[ServerEndpoint[Any, IO]]] = for {
    api <- apiEndpoints
    doc <- docEndpoints
  } yield api ++ doc
