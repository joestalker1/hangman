import Game._
import cats.Monad
import cats.implicits._

import scala.io.StdIn

trait Bot[F[_]] {
  def start(): F[Unit]
}

class SimpleBot[F[_] : Monad](wordDict: WordDict[F], game: Game[F], canBeMistaken: Int) extends Bot[F] {
  private val M = implicitly[Monad[F]]

  override def start(): F[Unit] = for {
    _ <- writeString(s"I try to guess your word using $canBeMistaken failed tries. Please type first,last characters and a word length space separated.\nFor example:a b 10")
    answer <- readString()
    word <- parseWord(answer)
    foundWords <- wordDict.findByFirstLastCharLen(word.first, word.last, word.len)
    _ <- game.buildTrie(foundWords)
    _ <- game.start(word, canBeMistaken)
    _ <- guessing(game)
  } yield ()

  private def guessedWord(s: String)(game: Game[F]): F[Unit] = for {
    _ <- writeString(s"Your word is '$s'. New game (y/n)?")
    yesno <- readString()
    _ <- if (yesno.toLowerCase == "y") start() else M.point(())
  } yield ()


  private def guessChar(char: Char)(game: Game[F]): F[Unit] = for {
    _ <- writeString(s"Do you have '$char'? Please type its position or -1 in other case.")
    _ <- writeString("If char occurs in a word a few times,please type its positions separated by space.")
    posOr <- readString()
    _ <- parseRespAndTellSuccOrFailedAttempt(char, posOr)(game)
    _ <- guessing(game)
  } yield ()

  private def parseRespAndTellSuccOrFailedAttempt(char: Char, posOr: String)(game: Game[F]): F[Unit] = {
    val nums = posOr.split(" ")
    var succ = false
    if (nums.nonEmpty) M.point(nums.map(_.toInt).foreach { pos =>
      succ = pos > -1
      tellSuccOrFailedAttempt(char, pos)
    }).flatMap(_ => if (succ) writeGuessedWord() else M.point())
    else {
       val pos = posOr.toInt
       tellSuccOrFailedAttempt(char, pos)
    }
  }

  private def tellSuccOrFailedAttempt(char: Char,pos: Int):F[Unit] = for {
    _ <- if (pos > -1) game.tell(SuccAttempt(char, pos)) else game.tell(FailedAttempt(char))
  } yield ()

  private def writeGuessedWord(): F[Unit] = for {
    word <- game.guessedWord()
    _ <- writeString(word)
  } yield ()

  private def guessing(game: Game[F]): F[Unit] =
    for {
      charOr <- game.guess()
      _ <- if (charOr.isEmpty) tellManyMistakes() else charOr.get.fold(guessedWord(_)(game), guessChar(_)(game))
    } yield ()

  private def tellManyMistakes(): F[Unit] = for {
    _ <- writeString(s"I've lost. I've made $canBeMistaken mistakes.")
    _ <- writeString("New game (y/n)?")
    yesno <- readString()
    _ <- if (yesno.toLowerCase == "y") start() else M.point(())
  } yield ()

  private def parseWord(s: String): F[Word] = M.point {
    val Array(ch1, ch2, len) = s.split(" ")
    Word(ch1(0), ch2(0), len.toInt)
  }

  private def readString(): F[String] = M.point(StdIn.readLine())

  private def writeString(s: String): F[Unit] = M.point(println(s))

}