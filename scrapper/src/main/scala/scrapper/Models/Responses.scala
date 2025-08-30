package scrapper.Models

import tethys.{JsonObjectWriter, JsonReader}

object Responses {
  case class ApiErrorResponse(
      description: String,
      code: String,
      exceptionName: Option[String],
      exceptionMessage: Option[String],
      stacktrace: Option[List[String]]
  )
  object ApiErrorResponse {
    implicit val apiErrorResponseReader: JsonReader[ApiErrorResponse]       = JsonReader.derived
    implicit val apiErrorResponseWriter: JsonObjectWriter[ApiErrorResponse] = JsonObjectWriter.derived
  }

  case class ListLinksResponse(links: List[LinkResponse], size: Int)
  object ListLinksResponse {
    implicit val apiErrorResponseReader: JsonReader[ListLinksResponse]       = JsonReader.derived
    implicit val apiErrorResponseWriter: JsonObjectWriter[ListLinksResponse] = JsonObjectWriter.derived
  }

  case class LinkResponse(id: Long, url: String, tags: List[String], filters: List[String])
  object LinkResponse {
    implicit val linkResponseReader: JsonReader[LinkResponse]       = JsonReader.derived
    implicit val linkResponseWriter: JsonObjectWriter[LinkResponse] = JsonObjectWriter.derived
  }
  case class NumberResponse(key: String, value: Long)
  object NumberResponse {
    implicit val numberResponseReader: JsonReader[NumberResponse]       = JsonReader.derived
    implicit val numberResponseWriter: JsonObjectWriter[NumberResponse] = JsonObjectWriter.derived
  }

  case class LinksDataResponse(data: Map[String, List[Long]])
  object LinksDataResponse {
    implicit val linksDataResponseReader: JsonReader[LinksDataResponse]       = JsonReader.derived
    implicit val linksDataResponseWriter: JsonObjectWriter[LinksDataResponse] = JsonObjectWriter.derived
  }
}
