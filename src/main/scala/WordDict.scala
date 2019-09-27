import cats.effect.Sync
import cats.implicits._

import scala.io.{BufferedSource, Source}
import scala.language.higherKinds

//word dictionary algebra
trait WordDict[F[_]] {
  def loadDict(): F[Either[Throwable, Unit]]

  def findByFirstLastCharLen(fromChar: Char, toChar: Char, expectedLen: Int): F[List[String]]
}

class FileWordDict[F[_] : Sync](private val fileNameList: List[String]) extends WordDict[F] {
  private var wordStorage: WordStorage = WordStorage()

  override def loadDict(): F[Either[Throwable, Unit]] =
    fileNameList.foreach { fileName =>
      Sync[F].bracket(Source.fromResource(fileName).pure[F])
      { file =>
        for (s <- file.getLines()) {
          //a word would contains some remarks after '/'
          val str = s.split("/")
          wordStorage = wordStorage + (if (str.nonEmpty) str(0) else s)
        }
        ().pure[F]
      }(_.close().pure[F])
    }.asRight.pure[F]


  override def findByFirstLastCharLen(fromChar: Char, toChar: Char, expectedLen: Int): F[List[String]] =
    if (wordStorage == null) List.empty.pure[F]
    else wordStorage.findByFirstLastCharLen(fromChar, toChar, expectedLen).pure[F]
}

