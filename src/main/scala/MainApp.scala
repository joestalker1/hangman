import cats.effect.IO

object MainApp extends App {
  type Effect[A] = IO[A]

  implicit val console = Console.console[Effect]()
  val program = for {
    config <- AppConfig.loadAll[Effect]()
    wordService = WordService.create[Effect](config.words)
    game = Game.create[Effect]()
    bot = new Bot[Effect](wordService, game, config.attempts)
    _ <- bot.start()
  } yield ()
  program.unsafeRunSync()
}

