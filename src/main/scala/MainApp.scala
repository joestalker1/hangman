import cats.effect.IO
import cats.implicits._

object MainApp extends App {
  type Effect[A] = IO[A]

  implicit val console = Console.console[Effect]()
  val program = for {
    config <- AppConfig.loadAll[Effect]()
    wordDict = new FileWordDict[Effect](config.words)
    game  = new HangmanGame[Effect]()
    bot = new Bot[Effect](wordDict, game, config.attempts)
    _ <- wordDict.loadDict()
    _ <- bot.start()
  } yield ()
  program.unsafeRunSync()
}

