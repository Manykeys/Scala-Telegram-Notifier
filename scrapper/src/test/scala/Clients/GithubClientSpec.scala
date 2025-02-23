package Clients

import Clients.GithubClient
import cats.effect.IO
import munit.CatsEffectSuite
import sttp.model.StatusCode

class GithubClientSpec extends CatsEffectSuite {
  test("getComments should return 200") {
    val client = GithubClient[IO]("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")
    client.getComments("octocat/Hello-World").map { response =>
      assertEquals(response, StatusCode.Ok)
    }
  }
}