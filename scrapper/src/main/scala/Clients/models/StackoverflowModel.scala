package Clients

import io.circe.generic.auto.*

case class Owner(
                  account_id: Int,
                  reputation: Int,
                  user_id: Int,
                  user_type: String,
                  profile_image: String,
                  display_name: String,
                  link: String
                )

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

case class StackOverflowResponse(
                                  items: List[Answer],
                                  has_more: Boolean,
                                  quota_max: Int,
                                  quota_remaining: Int
                                )
