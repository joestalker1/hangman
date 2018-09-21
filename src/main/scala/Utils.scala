import scala.io.{Source, StdIn}
import cats.effect.IO

/**
  * Utilities
  */
object Utils {
  //avoid to use cats.Resource
  def use[R](action: => R)(close: => Unit): R = try action finally close

  def openResource(fileName: String) = Source.fromResource(fileName)
}
