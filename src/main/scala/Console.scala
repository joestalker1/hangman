import cats.effect.Sync
import scala.io.StdIn

trait Console[F[_]] {
  def putString(s: String): F[Unit]

  def getString(): F[String]
}

object Console {
  def apply[F[_] : Sync](implicit console: Console[F]): Console[F] = console

  def console[F[_] : Sync](): Console[F] = new Console[F] {
    override def putString(s: String): F[Unit] = Sync[F].delay(println(s))

    override def getString(): F[String] = Sync[F].delay(StdIn.readLine())
  }
}