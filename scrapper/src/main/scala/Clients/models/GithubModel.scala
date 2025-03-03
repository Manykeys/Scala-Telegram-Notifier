package Clients

import io.circe.Decoder
import io.circe.generic.semiauto.*

case class User(
    login: String,
    id: Int,
    avatar_url: String,
    url: String,
    html_url: String
)

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
  implicit val userDecoder: Decoder[User]                   = deriveDecoder
  implicit val githubCommentDecoder: Decoder[GithubComment] = deriveDecoder
}
