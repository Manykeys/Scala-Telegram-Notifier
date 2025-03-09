package Clients

import cats.effect.IO
import munit.CatsEffectSuite
import sttp.client3.SttpBackend
import sttp.client3.impl.cats.CatsMonadError
import sttp.client3.testing.SttpBackendStub

class ClientFactorySpec extends CatsEffectSuite {
  val monad = new CatsMonadError[IO]
  val stubBackend: SttpBackend[IO, Any] = SttpBackendStub[IO, Any](monad)

  test("CommentsClientFactory should return Github client for repo identifier") {
    val client = CommentsClientFactory.create[IO]("fake", "owner/repo", stubBackend)
    assert(client.isInstanceOf[GithubCommentsClient[IO, _]])
  }

  test("CommentsClientFactory should return Stackoverflow client for numeric identifier") {
    val client = CommentsClientFactory.create[IO]("fake", "12345", stubBackend)
    assert(client.isInstanceOf[StackoverflowCommentsClient[IO, _]])
  }
}
