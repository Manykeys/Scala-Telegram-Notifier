package Clients

import cats.effect.IO
import io.circe.syntax.*
import munit.CatsEffectSuite
import sttp.client3.impl.cats.CatsMonadError
import sttp.client3.testing.SttpBackendStub

class StackoverflowClientSpec extends CatsEffectSuite {

  test("StackoverflowClient должен правильно парсить ответ StackExchange API") {
    val token    = "dummy-token"
    val question = "123"
    val fromDate = 1600000000

    val owner = Owner(
      account_id = 1,
      reputation = 100,
      user_id = 1,
      user_type = "registered",
      profile_image = "https://someurl.com/image.png",
      display_name = "Alice",
      link = "https://stackoverflow.com/users/1/alice"
    )

    val answer = Answer(
      owner = owner,
      is_accepted = true,
      score = 10,
      last_activity_date = 1600000010,
      last_edit_date = Some(1600000005),
      creation_date = 1600000000,
      answer_id = 101,
      question_id = question.toInt,
      content_license = "CC BY-SA 4.0"
    )

    val expectedResponse = StackOverflowResponse(
      items = List(answer),
      has_more = false,
      quota_max = 10000,
      quota_remaining = 9990
    )

    val jsonResponse = expectedResponse.asJson.noSpaces

    val monad = new CatsMonadError[IO]
    val backendStub: SttpBackendStub[IO, Any] = SttpBackendStub[IO, Any](monad)
      .whenRequestMatches { req =>
        req.uri.toString.contains(s"/questions/$question/answers") &&
        req.uri.toString.contains(s"fromdate=$fromDate")
      }
      .thenRespond(jsonResponse)

    val client = StackoverflowClient[IO, SttpBackendStub[IO, Any]](token)

    client.getComments(question, fromDate, backendStub).map { response =>
      assertEquals(response, expectedResponse)
    }
  }

  test("StackoverflowClient должен возвращать ошибку при недопустимом JSON") {
    val token    = "dummy-token"
    val question = "123"
    val fromDate = 1600000000

    val invalidJson = "invalid json"

    val monad = new CatsMonadError[IO]
    val backendStub: SttpBackendStub[IO, Any] = SttpBackendStub[IO, Any](monad)
      .whenRequestMatches { req =>
        req.uri.toString.contains(s"/questions/$question/answers")
      }
      .thenRespond(invalidJson)

    val client = StackoverflowClient[IO, SttpBackendStub[IO, Any]](token)

    client.getComments(question, fromDate, backendStub).attempt.map { result =>
      assert(result.isLeft, "Ожидается ошибка при некорректном JSON")
    }
  }
}
