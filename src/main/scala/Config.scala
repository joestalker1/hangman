import cats.effect.Sync
import com.typesafe.config.{Config, ConfigFactory}
import cats.implicits._

import scala.collection.JavaConverters._

case class AppConfig(words: List[String], attempts: Int) extends Product with Serializable

object AppConfig {
  def load[F[_] : Sync](fileName: String): F[AppConfig] =
    for {
      config <- Sync[F].delay(ConfigFactory.load(fileName))
      appConfg <- createAppConfig(config)
    } yield appConfg

  def loadAll[F[_] : Sync](): F[AppConfig] =
    for {
      config <- Sync[F].delay(ConfigFactory.load())
      appConfig <- createAppConfig(config)
    } yield appConfig

  private def createAppConfig[F[_] : Sync](config: Config): F[AppConfig] =
    (Sync[F].point(config.getStringList("words").asScala.toList), Sync[F].point(config.getInt("attempts"))).mapN(AppConfig.apply)
}