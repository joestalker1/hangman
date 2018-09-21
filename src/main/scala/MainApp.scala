import cats.effect.IO

object MainApp extends App {
  type Effect0[A] = IO[A]

  val canBeMistaken = 12
  val wordDict:WordDict[Effect0] = new FileWordDict[Effect0](List("dic.txt", "2of12.txt","3esl.txt"))
  val game: Game[Effect0] = new SimpleGame[Effect0]()
  val bot:Bot[Effect0] = new SimpleBot[Effect0](wordDict, game, canBeMistaken)

  (for {
    _ <- wordDict.loadDict()
    _ <- bot.start()
  } yield ()).unsafeRunSync()
}

