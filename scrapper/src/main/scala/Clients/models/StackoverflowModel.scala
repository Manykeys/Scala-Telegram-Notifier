package Clients

import io.circe.{Decoder, Encoder}
import io.circe.generic.auto.*
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class Owner(
    account_id: Int,
    reputation: Int,
    user_id: Int,
    user_type: String,
    profile_image: String,
    display_name: String,
    link: String
)

object Owner {
  implicit val githubCommentDecoder: Decoder[Owner] = deriveDecoder[Owner]
  implicit val githubCommentEncoder: Encoder[Owner] = deriveEncoder[Owner]
}


case class Answer(
    owner: Owner,
    is_accepted: Boolean,
    score: Int,
    last_activity_date: Int,
    last_edit_date: Option[Int],
    creation_date: Int,
    answer_id: Int,
    question_id: Int,
    content_license: String
)

object Answer {
  implicit val githubCommentDecoder: Decoder[Answer] = deriveDecoder[Answer]
  implicit val githubCommentEncoder: Encoder[Answer] = deriveEncoder[Answer]
}

case class StackOverflowResponse(
    items: List[Answer],
    has_more: Boolean,
    quota_max: Int,
    quota_remaining: Int
)

object StackOverflowResponse {
  implicit val githubCommentDecoder: Decoder[StackOverflowResponse] = deriveDecoder[StackOverflowResponse]
  implicit val githubCommentEncoder: Encoder[StackOverflowResponse] = deriveEncoder[StackOverflowResponse]
}