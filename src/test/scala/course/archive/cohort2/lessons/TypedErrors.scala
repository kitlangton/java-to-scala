package course.lessons

import zio._
import zio.ZIOAppDefault

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

object TypedErrors extends App {
  sealed trait AppError extends Throwable {
    def message: String
  }

  object AppError {
    final case class ParsingError(message: String) extends AppError
    final case class InputError(message: String)   extends AppError
  }

  def readInput: Either[AppError.InputError, String] =
    try Right(scala.io.StdIn.readLine("Enter a Number: "))
    catch {
      case _: Throwable => Left(AppError.InputError("Error reading from stdin"))
    }

  def parseInt(string: String): Either[AppError.ParsingError, Int] =
    try Right(string.trim.toInt)
    catch {
      case _: NumberFormatException =>
        Left(AppError.ParsingError(s"$string is not a number"))
    }

  def readInputFuture: Future[Either[AppError.InputError, String]] =
    Future(readInput)

  def parseIntFuture(string: String): Future[Either[AppError.ParsingError, Int]] =
    Future(parseInt(string))

  // Compose the semantics of Future and Either
  // Pure Composition
  // EitherT[Future, E, A]
  // Future[Either[E, A]] -> apply: EitherT[Future, E, A]
  // Either[E, A]         -> fromEither: EitherT[Future, E, A]
  // A                    -> leftT: EitherT[Future, E, A]
  // E                    -> rightT: EitherT[Future, E, A]
  // Future[A]            -> left: EitherT[Future, E, A]
  // Future[E]            -> right: EitherT[Future, E, A]
  val composed: EitherT[Future, AppError, Int] =
    for {
      stringX <- EitherT(readInputFuture).leftWiden[AppError]
      stringY <- EitherT.fromEither[Future, AppError, String](readInput)
      x       <- EitherT(parseIntFuture(stringX)).leftWiden[AppError]
      y       <- EitherT(parseIntFuture(stringY)).leftWiden[AppError]
    } yield x + y

  val futureNormal: Future[Int] = composed.value.flatMap {
    case Left(error)  => Future.failed(error)
    case Right(value) => Future.successful(value)
  }

  Await.result(composed.value, scala.concurrent.duration.Duration.Inf) match {
    case Left(error)  => println(s"Error: $error")
    case Right(value) => println(s"Result: $value")
  }
}

object TypedErrors2 extends ZIOAppDefault {
  sealed trait AppError extends Product with Serializable {
    def message: String
  }

  object AppError {
    final case class ParsingError(message: String) extends AppError
    final case class InputError(message: String)   extends AppError
  }

  def readInput: Either[AppError.InputError, String] =
    try Right(scala.io.StdIn.readLine("Enter a Number: "))
    catch {
      case _: Throwable => Left(AppError.InputError("Error reading from stdin"))
    }

  def parseInt(string: String): Either[AppError.ParsingError, Int] =
    try Right(string.trim.toInt)
    catch {
      case _: NumberFormatException =>
        Left(AppError.ParsingError(s"$string is not a number"))
    }

  // EitherT[Future, InputError, String]
  def readInputIO: IO[AppError.InputError, String] =
    ZIO.fromEither(readInput)

  def parseIntIO(string: String): IO[AppError.ParsingError, Int] =
    ZIO.fromEither(parseInt(string))

  // Compose the semantics of Future and Either
  // Pure Composition
  // EitherT[Future, E, A]
  // Future[Either[E, A]] -> apply: EitherT[Future, E, A]
  // Either[E, A]         -> fromEither: EitherT[Future, E, A]
  // A                    -> leftT: EitherT[Future, E, A]
  // E                    -> rightT: EitherT[Future, E, A]
  // Future[A]            -> left: EitherT[Future, E, A]
  // Future[E]            -> right: EitherT[Future, E, A]
  val composed: IO[AppError, Int] =
    for {
      stringX <- readInputIO
      stringY <- readInputIO
      x       <- parseIntIO(stringX)
      y       <- parseIntIO(stringY)
    } yield x + y

  val run =
    composed.debug("YAY")
}

// IO[+E, +A]

trait Monad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(fab: A => F[B]): F[B]

  def map[A, B](fa: F[A])(fab: A => B): F[B] =
    flatMap(fa)(a => pure(fab(a)))
}

object Monad {
  implicit val futureMonad: Monad[Future] =
    new Monad[Future] {
      override def pure[A](a: A): Future[A] =
        Future.successful(a)

      override def flatMap[A, B](fa: Future[A])(fab: A => Future[B]): Future[B] =
        fa.flatMap(fab)
    }

}
// MTL
final case class EitherT[F[_], E, A](value: F[Either[E, A]]) {

  def flatMap[B](
      f: A => EitherT[F, E, B]
  )(implicit F: Monad[F]): EitherT[F, E, B] =
    EitherT {
      F.flatMap(value) {
        case Left(error) => F.pure(Left(error))
        case Right(a)    => f(a).value
      }
    }

  def map[B](f: A => B)(implicit F: Monad[F]): EitherT[F, E, B] =
    EitherT {
      F.map(value) {
        case Left(error) => Left(error)
        case Right(a)    => Right(f(a))
      }
    }

  def widen[A1 >: A]: EitherT[F, E, A1] =
    this.asInstanceOf[EitherT[F, E, A1]]

  def leftWiden[E1 >: E]: EitherT[F, E1, A] =
    this.asInstanceOf[EitherT[F, E1, A]]
}

object EitherT {
  def fromEither[F[_], E, A](either: Either[E, A])(implicit monad: Monad[F]): EitherT[F, E, A] =
    EitherT(monad.pure(either))

  def rightT[F[_], E, A](value: A)(implicit monad: Monad[F]): EitherT[F, E, A] =
    EitherT(monad.pure(Right(value)))

  def leftT[F[_], E, A](value: E)(implicit monad: Monad[F]): EitherT[F, E, A] =
    EitherT(monad.pure(Left(value)))

  def right[F[_], E, A](fa: F[A])(implicit monad: Monad[F]): EitherT[F, E, A] =
    EitherT(monad.map(fa)(Right(_)))
}

final case class FutureEither[E, A](future: Future[Either[E, A]]) {
  def flatMap[B](
      f: A => FutureEither[E, B]
  ): FutureEither[E, B] =
    FutureEither {
      future.flatMap {
        case Left(error) => Future(Left(error))
        case Right(a)    => f(a).future
      }
    }

  def map[B](f: A => B): FutureEither[E, B] =
    FutureEither {
      future.map {
        case Left(error) => Left(error)
        case Right(a)    => Right(f(a))
      }
    }
}
