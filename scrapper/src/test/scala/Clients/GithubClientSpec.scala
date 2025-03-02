package Clients

import cats.effect.IO
import munit.CatsEffectSuite
import sttp.model.StatusCode

class GithubClientSpec extends CatsEffectSuite {
  test("getComments should return 401") {
    val client = GithubClient[IO]("")
    client.getComments("octocat/Hello-World").map { response =>
      assertEquals(response, "")
    }
  }
}