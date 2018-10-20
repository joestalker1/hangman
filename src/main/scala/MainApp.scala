import cats.effect.IO

object MainApp extends App {
  type Effect[A] = IO[A]

  val canBeMistaken = 12
  val wordDict: WordDict[Effect] = new FileWordDict[Effect](List("dic.txt", "2of12.txt","3esl.txt"))
  val game: Game[Effect] = new HangmanGame[Effect]()
  val bot = new Bot[Effect](wordDict, game, canBeMistaken)

  (for {
    _ <- wordDict.loadDict()
    _ <- bot.start()
  } yield ()).unsafeRunSync()
}

