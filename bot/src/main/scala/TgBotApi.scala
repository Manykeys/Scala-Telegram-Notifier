package bot

import cats.effect.IO
import io.circe.generic.auto.*
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.tethysjson.jsonBody
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import tethys.{JsonObjectWriter, JsonReader, JsonWriter}

object TgBotApi {

  case class ErrorResponse(
      description: String,
      code: String,
      exceptionName: String,
      exceptionMessage: String,
      stacktrace: List[String]
  )

  implicit val errorResponseJsonWriter: JsonWriter[ErrorResponse] = JsonObjectWriter.derived
  implicit val errorResponseJsonReader: JsonReader[ErrorResponse] = JsonReader.derived

  case class SendMessageRequest(id: Long, url: String, description: String, tgChatsIds: List[Long])

  implicit val sendMessageRequestJsonWriter: JsonWriter[SendMessageRequest] = JsonObjectWriter.derived
  implicit val sendMessageRequestJsonReader: JsonReader[SendMessageRequest] = JsonReader.derived

  val updatesEndpoint: PublicEndpoint[SendMessageRequest, ErrorResponse, String, Any] = endpoint.post
    .in("updates")
    .in(jsonBody[SendMessageRequest])
    .out(stringBody)
    .errorOut(
      statusCode(StatusCode.Ok).and(jsonBody[ErrorResponse])
    )
}

case class TgBotApi(bot: EchoBot[IO]) {

  import TgBotApi.*

  val updatesServerEndpoint: ServerEndpoint[Any, IO] = updatesEndpoint.serverLogic { request =>
    bot.update(request.tgChatsIds) *> IO.pure(Right("Ok"))
  }

  val apiEndpoints: List[ServerEndpoint[Any, IO]] = List(updatesServerEndpoint)

  val docEndpoints: List[ServerEndpoint[Any, IO]] = SwaggerInterpreter()
    .fromServerEndpoints[IO](apiEndpoints, "marxist-rooster", "1.0.0")

  val all: List[ServerEndpoint[Any, IO]] = apiEndpoints ++ docEndpoints
}
