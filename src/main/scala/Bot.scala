import Game._
import cats.Monad
import cats.effect.Sync
import cats.implicits._

/**
 * Implement computer logic to guess word that a human makes up.
 * @param wordDict
 * @param game
 * @param canBeMistaken
 * @tparam F
 */
class Bot[F[_] : Sync : Console](wordDict: WordService[F], game: Game[F], canBeMistaken: Int) {
  def start(): F[Unit] = for {
    _ <- Console[F].putString(s"I try to guess your word using $canBeMistaken failed tries. Please type first,last characters and a word length space separated.\nFor example:a b 10")
    answer <- Console[F].getString()
    word <- parseWord(answer)
    foundWords <- wordDict.findByFirstLastCharLen(word.first, word.last, word.len)
    _ <- game.buildTrie(foundWords)
    _ <- game.start(word, canBeMistaken)
    _ <- guessing(game)
  } yield ()

  private def guessedWord(s: String)(game: Game[F]): F[Unit] = for {
    _ <- Console[F].putString(s"Your word is '$s'. New game (y/n)?")
    yesno <- Console[F].getString()
    _ <- if (yesno.toLowerCase == "y") start() else ().pure[F]
  } yield ()

  private def guessChar(char: Char)(game: Game[F]): F[Unit] = for {
    _ <- Console[F].putString(s"Do you have '$char'? Please type char position started from 0, or -1 in other case.")
    _ <- Console[F].putString("If char occurs in a word a few times, please type its positions separated by space.")
    posOr <- Console[F].getString()
    _ <- parseRespAndTellSuccOrFailedAttempt(char, posOr)(game)
    _ <- guessing(game)
  } yield ()

  private def parseRespAndTellSuccOrFailedAttempt(char: Char, posOr: String)(game: Game[F]): F[Unit] = {
    val nums = posOr.split(" ")
    var succ = false
    if (nums.nonEmpty) Monad[F].point(nums.map(_.toInt).foreach { pos =>
      succ = pos > -1
      tellSuccOrFailedAttempt(char, pos)
    }).flatMap(_ => if (succ) writeGuessedWord() else ().pure[F])
    else {
      val pos = posOr.toInt
      tellSuccOrFailedAttempt(char, pos)
    }
  }

  private def tellSuccOrFailedAttempt(char: Char, pos: Int): F[Unit] = for {
    _ <- if (pos > -1) game.tell(SuccAttempt(char, pos)) else game.tell(FailedAttempt(char))
  } yield ()

  private def writeGuessedWord(): F[Unit] = for {
    word <- game.guessedWord()
    _ <- Console[F].putString(word)
  } yield ()

  private def guessing(game: Game[F]): F[Unit] =
    for {
      charOr <- game.guess()
      _ <- if (charOr.isEmpty) tellManyMistakes() else charOr.get.fold(guessedWord(_)(game), guessChar(_)(game))
    } yield ()

  private def tellManyMistakes(): F[Unit] = for {
    _ <- Console[F].putString(s"I've lost. I've made $canBeMistaken mistakes.")
    _ <- Console[F].putString("New game (y/n)?")
    yesno <- Console[F].getString()
    _ <- if (yesno.toLowerCase == "y") start() else ().pure[F]
  } yield ()

  private def parseWord(s: String): F[Word] = {
    val Array(firstChar, lastChar, wordLen) = s.split(" ")
    Word(firstChar(0), lastChar(0), wordLen.toInt)
    }.pure[F]
}