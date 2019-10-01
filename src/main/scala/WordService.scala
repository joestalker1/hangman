import cats.effect.Sync
import cats.implicits._

import scala.io.Source
import scala.language.higherKinds
import WordStorage._

trait WordService[F[_]] {
  def findByFirstLastCharLen(fromChar: Char, toChar: Char, expectedLen: Int): F[List[String]]
}

object WordService {
  def create[F[_] : Sync](fileNameList: List[String]) = new WordService[F] {
    private lazy val wordStorage: WordStorage = loadWords().foldMap(identity[WordStorage])

    def loadWords(): List[WordStorage] = {
      for {
        fileName <- fileNameList
      } yield {
        val source = Source.fromResource(fileName)
        try {
          val lines = for (s <- source.getLines()) yield {
            //a word would contains some remarks after '/'
            val str = s.split("/")
            if (str.nonEmpty) str(0) else s
          }
          WordStorage(lines.toList)
        } finally {
          source.close()
        }
      }
    }

    override def findByFirstLastCharLen(fromChar: Char, toChar: Char, expectedLen: Int): F[List[String]] = {
      if (wordStorage == null) List.empty.pure[F]
      else wordStorage.findByFirstLastCharLen(fromChar, toChar, expectedLen).pure[F]
    }
  }
}
