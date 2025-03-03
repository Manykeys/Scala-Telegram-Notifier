package Repository

import cats.effect.IO
import munit.CatsEffectSuite
import scrapper.Endpoints.{AddLinkRequest, LinkResponse}
import scrapper.repository.LinkRepository

class LinkRepositoryTest extends CatsEffectSuite {

  test("should create a new repository and register a chat") {
    for {
      repo  <- LinkRepository.create[IO]
      _     <- repo.registerChat(123)
      links <- repo.getLinks(123)
    } yield {
      assertEquals(links, List.empty)
    }
  }

  test("should add a new link to an existing chat") {
    val addLinkRequest = AddLinkRequest(
      link = "https://example.com",
      tags = List("tag1", "tag2"),
      filters = List("filter1", "filter2")
    )

    for {
      repo <- LinkRepository.create[IO]
      _    <- repo.registerChat(123)

      addedLinkOpt  <- repo.addLink(123, addLinkRequest.link, addLinkRequest.tags, addLinkRequest.filters)
      linksAfterAdd <- repo.getLinks(123)
    } yield {
      assert(addedLinkOpt.isDefined)
      assertEquals(linksAfterAdd.size, 1)
      assertEquals(linksAfterAdd.head.url, "https://example.com")
      assertEquals(linksAfterAdd.head.tags, List("tag1", "tag2"))
      assertEquals(linksAfterAdd.head.filters, List("filter1", "filter2"))
    }
  }

  test("should return None when trying to add a link to a non-existing chat") {
    val addLinkRequest = AddLinkRequest(
      link = "https://example.com",
      tags = List("tag1"),
      filters = List("filter1")
    )

    for {
      repo <- LinkRepository.create[IO]

      addedLinkOpt <- repo.addLink(123, addLinkRequest.link, addLinkRequest.tags, addLinkRequest.filters)
    } yield {
      assert(addedLinkOpt.isEmpty)
    }
  }
}
