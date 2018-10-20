import cats.Monad
import cats.implicits._

import scala.language.higherKinds
import Game._

trait Game[F[_]] {
  def buildTrie(list: List[String]): F[Unit]

  def start(word: Word, failedTries: Int): F[Unit]

  def guess(): F[Option[Either[String, Char]]]

  def tell(answer: Answer): F[Unit]

  def guessedWord():F[String]
}

object Game {
  sealed trait Ask

  case class Word(first: Char, last: Char, len: Int) extends Ask

  sealed trait Answer
  case class FailedAttempt(char: Char) extends Answer
  case class SuccAttempt(char: Char, pos: Int) extends Answer
}

class HangmanGame[F[_] : Monad] extends Game[F] {
  type GuessRes = Option[Either[String, Char]]

  private var trie = Trie()
  private var wordLen = 0
  private var canBeMistaken = 0

  override def buildTrie(list: List[String]): F[Unit] =
    Monad[F].point {
      trie = Trie()
      for (s <- list) {
        trie.insert(s)
      }
    }

  override def start(word: Word, failedTries: Int): F[Unit] = {
    trie.markAsGuessed(word.first, 0)
    trie.markAsGuessed(word.last, word.len - 1)
    wordLen = word.len
    canBeMistaken = failedTries
  }.pure[F]

  override def guess(): F[GuessRes] = {
    if(canBeMistaken == 0) (None:GuessRes).pure[F]
    else{
      val realLen = trie.guessedSize()
      if (wordLen == realLen) (Some(trie.guessedString(wordLen).asLeft):GuessRes).pure[F]
      else (Some(trie.guess().fold(throw new RuntimeException("Unexpected behaviour"))(identity[Char]).asRight):GuessRes).pure[F]
    }
  }

  override def tell(answer: Answer): F[Unit] = {
    answer match {
      case FailedAttempt(char) => trie.markAsMissing(char)
                                  canBeMistaken -= 1
      case SuccAttempt(char, pos) => trie.markAsGuessed(char, pos)
    }
  }.pure[F]

  override def guessedWord():F[String] = trie.guessedString(wordLen).pure[F]
}

