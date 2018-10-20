import cats.Monad
import cats.effect.Bracket
import cats.implicits._

import scala.io.{BufferedSource, Source}
import scala.language.higherKinds

//word dictionary algebra
trait WordDict[F[_]] {
  def loadDict(): F[Either[Throwable, Unit]]

  def findByFirstLastCharLen(fromChar: Char, toChar: Char, expectedLen: Int): F[List[String]]
}

import Utils._

class FileWordDict[F[_] : Monad](private val fileNameList: List[String]) extends WordDict[F] {
  private var wordStorage: WordStorage = WordStorage()

  override def loadDict(): F[Either[Throwable, Unit]] =
    (fileNameList.foreach { fileName =>
      val file = openResource(fileName)
      use {
        for (s <- file.getLines()) {
          //a word would contains some remarks after '/'
          val str = s.split("/")
          wordStorage = wordStorage + (if (str.nonEmpty) str(0) else s)
        }
      }(release(file))
    }.asRight).pure[F]


  override def findByFirstLastCharLen(fromChar: Char, toChar: Char, expectedLen: Int): F[List[String]] =
    (
      if (wordStorage == null) List.empty
      else wordStorage.findByFirstLastCharLen(fromChar, toChar, expectedLen)
    ).pure[F]

  private def release(source: Source): Unit = source.close()
}

