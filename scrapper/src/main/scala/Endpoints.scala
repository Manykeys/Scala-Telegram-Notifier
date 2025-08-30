package scrapper

import cats.effect.IO
import org.slf4j.{Logger, LoggerFactory}
import scrapper.Models.Requests.*
import scrapper.Models.Responses.*
import scrapper.repository.LinkRepository
import sttp.tapir.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.tethysjson.jsonBody
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.swagger.bundle.SwaggerInterpreter

object Endpoints:
  val logger: Logger = LoggerFactory.getLogger(getClass)

  def registerChatLogic(id: Long, repository: LinkRepository[IO]): IO[Either[ApiErrorResponse, Unit]] =
    repository.registerChat(id).map { _ =>
      logger.info(s"Registered chat with ID $id")
      Right(())
    }

  def deleteChatLogic(id: Long, repository: LinkRepository[IO]): IO[Either[ApiErrorResponse, Unit]] =
    repository.deleteChat(id).map { _ =>
      logger.info(s"Deleted chat with ID $id")
      Right(())
    }

  def getLinksLogic(chatId: Long, repository: LinkRepository[IO]): IO[Either[ApiErrorResponse, ListLinksResponse]] =
    repository.getLinks(chatId).map { links =>
      logger.info(s"Fetched links for chat $chatId: ${links.size}")
      Right(ListLinksResponse(links, links.size))
    }

  def addLinkLogic(
      chatId: Long,
      request: AddLinkRequest,
      repository: LinkRepository[IO]
  ): IO[Either[ApiErrorResponse, LinkResponse]] =
    for {
      existingLinks <- repository.getLinks(chatId)
      result <- if (existingLinks.exists(_.url == request.link)) {
        IO.pure(
          Left(ApiErrorResponse("Duplicate link", "409", None, Some("This link already exists"), None))
        )
      } else {
        repository.addLink(chatId, request.link, request.tags, request.filters).map {
          case Some(link) =>
            logger.info(s"Added link for chat $chatId: ${link.url}")
            Right(link)
          case None =>
            Left(ApiErrorResponse("Link not added", "404", None, None, None))
        }
      }
    } yield result

  def removeLinkLogic(
      chatId: Long,
      request: RemoveLinkRequest,
      repository: LinkRepository[IO]
  ): IO[Either[ApiErrorResponse, LinkResponse]] =
    repository.removeLink(chatId, request.link).map {
      case Some(link) =>
        logger.info(s"Removed link for chat $chatId: ${link.url}")
        Right(link)
      case None =>
        Left(ApiErrorResponse("Link not found", "404", None, None, None))
    }

  def getAllLinksLogic(repository: LinkRepository[IO]): IO[Either[ApiErrorResponse, LinksDataResponse]] =
    repository.getAllUrlData.map {
      case data if data.nonEmpty => Right(LinksDataResponse(data))
      case _                     => Left(ApiErrorResponse("No data found", "404", None, None, None))
    }

  def addNumberLogic(
      request: AddNumberRequest,
      repository: LinkRepository[IO]
  ): IO[Either[ApiErrorResponse, NumberResponse]] =
    repository.addNumber(request.key, request.value) *> IO {
      logger.info(s"Added number for key ${request.key}: ${request.value}")
      Right(NumberResponse(request.key, request.value))
    }

  def getNumberLogic(
      key: String,
      repository: LinkRepository[IO]
  ): IO[Either[ApiErrorResponse, NumberResponse]] =
    repository.getNumber(key).map {
      case Some(value) =>
        logger.info(s"Fetched number for key $key: $value")
        Right(NumberResponse(key, value))
      case None =>
        Left(ApiErrorResponse("Number not found", "404", None, None, None))
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
    addNumber = endpoint.post
      .in("number")
      .in(jsonBody[AddNumberRequest])
      .errorOut(jsonBody[ApiErrorResponse])
      .out(jsonBody[NumberResponse])
      .serverLogic { request =>
        addNumberLogic(request, repository)
      }
    getNumber = endpoint.get
      .in("number" / path[String]("key"))
      .errorOut(jsonBody[ApiErrorResponse])
      .out(jsonBody[NumberResponse])
      .serverLogic { key =>
        getNumberLogic(key, repository)
      }
  } yield List(registerChat, deleteChat, getLinks, addLink, removeLink, getAllLinks, addNumber, getNumber)

  val apiEndpoints: IO[List[ServerEndpoint[Any, IO]]] = allEndpoints

  val docEndpoints: IO[List[ServerEndpoint[Any, IO]]] = apiEndpoints.flatMap { api =>
    IO(SwaggerInterpreter().fromServerEndpoints[IO](api, "Scrapper API", "1.0.0"))
  }

  val all: IO[List[ServerEndpoint[Any, IO]]] = for {
    api <- apiEndpoints
    doc <- docEndpoints
  } yield api ++ doc
