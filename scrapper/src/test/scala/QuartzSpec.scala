package scrapper

import Clients.GithubComment
import QuartzScheduler.*
import QuartzScheduler.Models.*
import cats.effect.IO
import munit.CatsEffectSuite
import scrapper.Models.Responses.*
import sttp.model.Uri

import java.time.Instant

class QuartzSpec extends CatsEffectSuite {
  val dummyConfig = ServiceConf(apiPort = 8080, tgPort = 8081, githubToken = "dummyToken")
  test("Notifier.updateNumberAndNotify отправляет уведомление только для пользователей, следящих за ссылкой") {
    val dummyNumberUpdater = new NumberUpdater {
      override def updateNumber(apiUri: Uri, payload: NumberUpdatePayload): IO[String] =
        IO.pure("number updated")
    }

    var telegramCalled = false

    val dummyTelegramNotifier = new TelegramNotifier {
      override def notifyTelegram(tgUri: Uri, payload: TelegramNotificationPayload): IO[String] = {
        telegramCalled = true
        IO.pure("telegram notified")
      }
    }

    val tgUri    = Uri.parse("http://telegram:8080").getOrElse(sys.error("Invalid URI"))
    val notifier = new Notifier(dummyNumberUpdater, dummyTelegramNotifier, tgUri)
    val apiUri   = Uri.parse("http://api:8080").getOrElse(sys.error("Invalid URI"))

    val linksDataWithUser = LinksDataResponse(Map("repo1" -> List(12345L)))
    for {
      result1 <- notifier.updateNumberAndNotify(apiUri, "repo1", 1000L, linksDataWithUser)
      _ = assertEquals(result1._1, "number updated")
      _ = assertEquals(result1._2, Some("telegram notified"))
      _ = assert(telegramCalled)

      _ = telegramCalled = false

      result2 <- notifier.updateNumberAndNotify(apiUri, "repo2", 1000L, linksDataWithUser)
      _ = assertEquals(result2._1, "number updated")
      _ = assertEquals(result2._2, None)
      _ = assert(!telegramCalled)
    } yield ()
  }

  test(
    "processGithubComments отправляет обновление, если новый timestamp отличается и пользователь следит за ссылкой"
  ) {
    val dummyLinksDataFetcher = new LinksDataFetcher(null) {
      override def fetchCurrentNumber(apiUri: Uri, identifier: String): IO[Option[NumberResponse]] =
        IO.pure(Some(NumberResponse(identifier, 0)))
    }

    var telegramCalled = false

    val dummyNumberUpdater = new NumberUpdater {
      override def updateNumber(apiUri: Uri, payload: NumberUpdatePayload): IO[String] =
        IO.pure("number updated")
    }
    val dummyTelegramNotifier = new TelegramNotifier {
      override def notifyTelegram(tgUri: Uri, payload: TelegramNotificationPayload): IO[String] = {
        telegramCalled = true
        IO.pure("telegram notified")
      }
    }
    val tgUri    = Uri.parse("http://telegram:8080").getOrElse(sys.error("Invalid URI"))
    val notifier = new Notifier(dummyNumberUpdater, dummyTelegramNotifier, tgUri)

    val newTimestampStr = "2025-03-14T10:00:00Z"
    val dummyComment = GithubComment(
      url = "http://example.com/comment/1",
      id = 1,
      diff_hunk = "dummy",
      path = "dummy",
      commit_id = "dummy",
      user = None,
      body = "dummy",
      created_at = newTimestampStr,
      updated_at = newTimestampStr,
      html_url = "http://example.com/comment/1/html",
      pull_request_url = "http://example.com/pr",
      author_association = "dummy",
      line = None,
      position = None,
      subject_type = "dummy"
    )
    val comments  = List(dummyComment)
    val linksData = LinksDataResponse(Map("repo1" -> List(12345L)))
    val apiUri    = Uri.parse("http://api:8080").getOrElse(sys.error("Invalid URI"))

    for {
      result <- new GithubCommentsProcessor(null, dummyConfig, dummyLinksDataFetcher, notifier)
        .processGithubComments(apiUri, tgUri, "repo1", comments, linksData)
      _ = assertEquals(result, Updated(Instant.parse(newTimestampStr).toEpochMilli))
      _ = assert(telegramCalled)
    } yield ()
  }

  test("processGithubComments не отправляет обновление, если комментариев нет") {
    val dummyLinksDataFetcher = new LinksDataFetcher(null) {
      override def fetchCurrentNumber(apiUri: Uri, identifier: String): IO[Option[NumberResponse]] =
        IO.pure(Some(NumberResponse(identifier, 1000)))
    }
    var telegramCalled = false
    val dummyNumberUpdater = new NumberUpdater {
      override def updateNumber(apiUri: Uri, payload: NumberUpdatePayload): IO[String] =
        IO.pure("number updated")
    }
    val dummyTelegramNotifier = new TelegramNotifier {
      override def notifyTelegram(tgUri: Uri, payload: TelegramNotificationPayload): IO[String] = {
        telegramCalled = true
        IO.pure("telegram notified")
      }
    }
    val tgUri    = Uri.parse("http://telegram:8080").getOrElse(sys.error("Invalid URI"))
    val notifier = new Notifier(dummyNumberUpdater, dummyTelegramNotifier, tgUri)
    val apiUri   = Uri.parse("http://api:8080").getOrElse(sys.error("Invalid URI"))

    val comments  = List.empty[GithubComment]
    val linksData = LinksDataResponse(Map("repo1" -> List(12345L)))

    for {
      result <- new GithubCommentsProcessor(null, dummyConfig, dummyLinksDataFetcher, notifier)
        .processGithubComments(apiUri, tgUri, "repo1", comments, linksData)
      _ = assertEquals(result, NoComments)
      _ = assert(!telegramCalled)
    } yield ()
  }
}
