package scrapper.repository

import cats.effect.{Ref, Sync}
import cats.implicits.*
import scrapper.Endpoints.LinkResponse


trait LinkRepository[F[_]] {
  def registerChat(chatId: Long): F[Unit]
  def deleteChat(chatId: Long): F[Unit]
  def getLinks(chatId: Long): F[List[LinkResponse]]
  def addLink(chatId: Long, url: String, tags: List[String], filters: List[String]): F[Option[LinkResponse]]
  def removeLink(chatId: Long, url: String): F[Option[LinkResponse]]
  def getUrlData(url: String): F[Option[List[List[Long]]]]
  def getAllUrlData: F[Map[String, List[List[Long]]]]
}

class LinkRepositoryImpl[F[_]: Sync](
                                      ref: Ref[F, Map[Long, List[LinkResponse]]],
                                      urlRef: Ref[F, Map[String, List[List[Long]]]]
                                    ) extends LinkRepository[F] {

  override def registerChat(chatId: Long): F[Unit] =
    ref.update { links =>
      if links.contains(chatId) then links
      else links.updated(chatId, List.empty)
    }

  override def deleteChat(chatId: Long): F[Unit] =
    ref.update(_ - chatId)

  override def getLinks(chatId: Long): F[List[LinkResponse]] =
    ref.get.map(_.getOrElse(chatId, List.empty))

  override def addLink(chatId: Long, url: String, tags: List[String], filters: List[String]): F[Option[LinkResponse]] =
    for {
      newLink <- Sync[F].delay(LinkResponse(System.nanoTime(), url, tags, filters))
      result <- ref.modify { links =>
        links.get(chatId) match {
          case Some(existingLinks) =>
            val updatedLinks = newLink :: existingLinks
            (links.updated(chatId, updatedLinks), Some(newLink))
          case None =>
            (links, None)
        }
      }
      _ <- urlRef.update { urlMap =>
        val updatedList = List(chatId) :: urlMap.getOrElse(url, List.empty)
        urlMap.updated(url, updatedList)
      }.whenA(result.isDefined)
    } yield result

  override def removeLink(chatId: Long, url: String): F[Option[LinkResponse]] =
    for {
      result <- ref.modify { links =>
        links.get(chatId) match {
          case Some(existingLinks) =>
            existingLinks.find(_.url == url) match {
              case Some(link) =>
                val updatedLinks = existingLinks.filterNot(_.url == url)
                (links.updated(chatId, updatedLinks), Some(link))
              case None =>
                (links, None)
            }
          case None =>
            (links, None)
        }
      }
      _ <- urlRef.update { urlMap =>
        urlMap.get(url) match {
          case Some(entries) =>
            val updatedEntries = entries.filterNot(_ equals chatId)
            if (updatedEntries.isEmpty) urlMap - url else urlMap.updated(url, updatedEntries)
          case None => urlMap
        }
      }.whenA(result.isDefined)
    } yield result

  override def getUrlData(url: String): F[Option[List[List[Long]]]] =
    urlRef.get.map(_.get(url))

  override def getAllUrlData: F[Map[String, List[List[Long]]]] =
    urlRef.get
}

object LinkRepository {
  def create[F[_]: Sync]: F[LinkRepository[F]] =
    for {
      chatRef <- Ref.of[F, Map[Long, List[LinkResponse]]](Map.empty)
      urlRef  <- Ref.of[F, Map[String, List[List[Long]]]](Map.empty)
    } yield new LinkRepositoryImpl[F](chatRef, urlRef)
}
