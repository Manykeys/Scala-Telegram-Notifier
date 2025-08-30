package Models

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

object Responses {
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

  case class PendingTrack(link: String, tags: Option[List[String]] = None, filters: Option[List[String]] = None)
}
