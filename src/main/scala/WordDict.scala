import cats.Monad
import cats.implicits._

import scala.io.Source
import scala.language.higherKinds

//word dictionary algebra
trait WordDict[F[_]] {
  def loadDict(): F[Either[Throwable, Unit]]

  def findByFirstLastCharLen(fromChar: Char, toChar: Char, expectedLen: Int): F[List[String]]
}

import Utils._

class FileWordDict[F[_] : Monad](private val fileNameList: List[String]) extends WordDict[F] {
  private val M = implicitly[Monad[F]]
  private var wordStorage: WordStorage = null

  override def loadDict(): F[Either[Throwable, Unit]] = {
    wordStorage = WordStorage()
    M.point(fileNameList.foreach{ fileName =>
      val file = openResource(fileName)
      use {
        for (s <- file.getLines()) {
          //a word would contains some remarks after '/'
          val str = s.split("/")
          wordStorage = wordStorage + (if (str.nonEmpty) str(0) else s)
        }
      }(release(file))
    }.asRight)
  }

  override def findByFirstLastCharLen(fromChar: Char, toChar: Char, expectedLen: Int): F[List[String]] =
    M.point(
      if (wordStorage == null) List.empty
      else wordStorage.findByFirstLastCharLen(fromChar, toChar, expectedLen)
    )

  private def release(source: Source): Unit = source.close()
}

