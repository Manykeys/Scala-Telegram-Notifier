package Clients

import io.circe.generic.semiauto.*
import io.circe.{Decoder, Encoder}

case class User(
    login: String,
    id: Int,
    avatar_url: String,
    url: String,
    html_url: String
)

object User {
  implicit val userDecoder: Decoder[User] = deriveDecoder[User]
  implicit val userEncoder: Encoder[User] = deriveEncoder[User]
}

case class GithubComment(
    url: String,
    id: Int,
    diff_hunk: String,
    path: String,
    commit_id: String,
    user: Option[User],
    body: String,
    created_at: String,
    updated_at: String,
    html_url: String,
    pull_request_url: String,
    author_association: String,
    line: Option[Int],
    position: Option[Int],
    subject_type: String
)

object GithubComment {
  implicit val githubCommentDecoder: Decoder[GithubComment] = deriveDecoder[GithubComment]
  implicit val githubCommentEncoder: Encoder[GithubComment] = deriveEncoder[GithubComment]
}
