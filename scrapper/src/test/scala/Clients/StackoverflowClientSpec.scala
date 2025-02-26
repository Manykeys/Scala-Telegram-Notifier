package Clients

import cats.effect.IO
import munit.CatsEffectSuite
import sttp.model.StatusCode

class StackoverflowClientSpec extends CatsEffectSuite {
  test("getComments should return 200") {
    val client = StackoverflowClient[IO]("")
    client.getComments("20218684", 0).map { response =>
      assertEquals(response, StatusCode.Ok)
    }
  }
}
