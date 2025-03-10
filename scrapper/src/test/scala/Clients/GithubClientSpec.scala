package Clients

import cats.effect.IO
import io.circe.syntax.*
import munit.CatsEffectSuite
import sttp.client3.Response
import sttp.client3.impl.cats.CatsMonadError
import sttp.client3.testing.SttpBackendStub

class GithubClientSpec extends CatsEffectSuite {

  test("GithubClient возвращает комментарии с первой страницы, если вторая пустая") {
    val token = "dummy-token"
    val repo  = "owner/repo"

    val commentsList = List(
      GithubComment(
        url = "https://api.github.com/repos/owner/repo/pulls/comments/1",
        id = 1,
        diff_hunk = "@@ -1,5 +1,5 @@\n- some code\n+ some updated code",
        path = "src/main/scala/Main.scala",
        commit_id = "1234567890abcdef1234567890abcdef12345678",
        user = Some(User(
          login = "john_doe",
          id = 42,
          avatar_url = "https://avatars.githubusercontent.com/u/42?v=4",
          url = "https://api.github.com/users/john_doe",
          html_url = "https://github.com/john_doe"
        )),
        body = "This is a comment on the pull request.",
        created_at = "2023-03-09T12:34:56Z",
        updated_at = "2023-03-09T12:35:10Z",
        html_url = "https://github.com/owner/repo/pull/1#discussion_r123456789",
        pull_request_url = "https://api.github.com/repos/owner/repo/pulls/1",
        author_association = "COLLABORATOR",
        line = Some(10),
        position = Some(5),
        subject_type = "PR"
      )
    )

    val jsonPage1 = commentsList.asJson.noSpaces
    val jsonPage2 = "[]"

    val monad = new CatsMonadError[IO]
    val backendStub: SttpBackendStub[IO, Any] =
      SttpBackendStub[IO, Any](monad)
        .whenRequestMatchesPartial {
          case req if req.uri.toString.contains("&page=1") =>
            Response.ok(jsonPage1)
          case req if req.uri.toString.contains("&page=2") =>
            Response.ok(jsonPage2)
        }

    val githubClient = GithubClient[IO, SttpBackendStub[IO, Any]](token)

    for {
      comments <- githubClient.getComments(repo, backendStub)
    } yield {
      assertEquals(comments, commentsList)
    }
  }
}
