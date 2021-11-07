package course

import zio.test._
import zio.test.environment.TestEnvironment

import scala.annotation.implicitNotFound

trait Lesson extends DefaultRunnableSpec {
  type ???

  def exercise: ZSpec[TestEnvironment, Any]

  override def spec: ZSpec[TestEnvironment, Any] = exercise
}

object Lesson {
  type ??? = Nothing
}

@implicitNotFound("${B} is not the right type")
trait SameType[-A, +B]

object SomeType {
  implicit def same[A, B](implicit ev: A <:< B): SameType[A, B] =
    new SameType[A, B] {}
}

final case class Guess[+A]()

object Guess {
  def apply[A]: Guess[A] = new Guess()

  def guessType[A, B](f: A)(guess: Guess[B])(implicit ev: A SameType B): Unit = {
    val _ = (ev, f, guess)
  }
}
