package scrapper.Models

import tethys.{JsonObjectWriter, JsonReader}

object Requests {
  case class AddLinkRequest(link: String, tags: List[String], filters: List[String])

  object AddLinkRequest {
    implicit val AddLinkRequestReader: JsonReader[AddLinkRequest]       = JsonReader.derived
    implicit val AddLinkRequestWriter: JsonObjectWriter[AddLinkRequest] = JsonObjectWriter.derived
  }

  case class RemoveLinkRequest(link: String)

  object RemoveLinkRequest {
    implicit val RemoveLinkReader: JsonReader[RemoveLinkRequest]       = JsonReader.derived
    implicit val RemoveLinkWriter: JsonObjectWriter[RemoveLinkRequest] = JsonObjectWriter.derived
  }
  case class AddNumberRequest(key: String, value: Long)

  object AddNumberRequest {
    implicit val AddNumberRequestReader: JsonReader[AddNumberRequest]       = JsonReader.derived
    implicit val AddNumberRequestWriter: JsonObjectWriter[AddNumberRequest] = JsonObjectWriter.derived
  }
}
