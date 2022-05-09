package course.exercises

import zio._
import zio.test._

import scala.annotation.implicitNotFound
import scala.concurrent.Future

trait Exercise extends ZIOSpecDefault {
  type ???

  def exercise: ZSpec[TestEnvironment, Any]

  override def spec: ZSpec[TestEnvironment, Any] = exercise

  def testFuture[A](future: Future[A])(f: Either[Throwable, A] => TestResult): Task[TestResult] =
    ZIO.fromFuture(_ => future).either.map { result =>
      f(result)
    }
}

object Exercise {
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
