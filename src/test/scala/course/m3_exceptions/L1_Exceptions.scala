package course.m3_exceptions

import course.Lesson
import zio.test._
import zio.test.TestAspect._

/** Many programming languages use exceptions as a way to short-circuit the
  * normal return process and signal failure to higher levels of an application.
  * Functional Scala provides another option: using typed return values, which
  * allow the compiler help you handle expected error cases, resulting in much
  * more robust and resilient code that better deals with the complexity of life
  * in the cloud.
  *
  * In this module, you will learn how to eliminate exceptions from your
  * application and program using typed return values.
  */

object ExceptionConstructors extends Lesson {

  /** ✏ EXERCISE
    *
    * Modify `parseInt` to return an `Option`.
    */
  val optionTest = test("Option") {
    def parseInt(s: String) = s.toInt

    def test = (parseInt(""): Any) match {
      case None => "None"
      case _    => "Some"
    }

    assertTrue(test == "None")
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Modify `parseInt` to return a `Try`.
    */
  val tryTest = test("Try") {
    import scala.util._

    def parseInt(s: String) = s.toInt

    def test = (parseInt(""): Any) match {
      case Failure(_) => "Failure"
      case _          => "Success"
    }

    assertTrue(test == "Failure")
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Modify `parseInt` to return an `Either`, where `Left` indicates failure to
    * parse an integer.
    */
  val eitherTest = test("Either") {
    def parseInt(s: String) = s.toInt

    def test = (parseInt(""): Any) match {
      case Left(_) => "Left"
      case _       => "Right"
    }

    assertTrue(test == "Left")
  } @@ ignore

  def exercise =
    suite("Constructors")(
      optionTest,
      tryTest,
      eitherTest
    )
}

object MappingExceptions extends Lesson {

  /** ✏ EXERCISE
    *
    * Using `Option##map`, use the `parseInt` helper function to implement a
    * correct `Id` constructor.
    */
  val optionMapTest = test("Option") {
    def parseInt(i: String): Option[Int] =
      try Some(i.toInt)
      catch { case _: Throwable => None }

    final case class Id private (value: Int)

    object Id {
      def fromString(value: String): Option[Id] = {
        parseInt(value)

        ???
      }
    }

    assertTrue(Id.fromString("123").isDefined)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `Try#map`, use the `parseInt` helper function to implement a correct
    * `Natural.fromString` constructor, which will succeed only if the string is
    * a number, and if that number is non-negative.
    */
  val tryMapTest = test("Try") {
    import scala.util._

    def parseInt(i: String): Try[Int] = Try(i.toInt)

    final case class Id private (value: Int)

    object Id {
      def fromString(value: String): Try[Id] = {
        parseInt(value)

        ???
      }
    }

    assertTrue(Id.fromString("123").isSuccess)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `Either#map`, use the `parseInt` helper function to implement a
    * correct `Natural.fromString` constructor, which will succeed only if the
    * string is a number, and if that number is non-negative.
    */
  val either = test("Either") {
    def parseInt(i: String): Either[String, Int] =
      try Right(i.toInt)
      catch {
        case e: NumberFormatException => Left(e.getMessage())
      }

    final case class Id private (value: Int)

    object Id {
      def fromString(value: String): Either[String, Id] = {
        parseInt(value)

        ???
      }
    }

    assertTrue(Id.fromString("123").isRight)
  } @@ ignore

  def exercise =
    suite("Mapping Exceptions")(
      optionMapTest,
      tryMapTest,
      either
    )
}

object Fallbacks extends Lesson {

  /** ✏ EXERCISE
    *
    * Implement `fallback` in such a way that it prefers the left hand side, if
    * it contains a value, otherwise, it will use the right hand side.
    */
  val optionFallbackTest = test("Option") {
    def fallback[A](left: Option[A], right: Option[A]): Option[A] = ???

    assertTrue(fallback(None, Some(42)) == Some(42))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Implement `fallback` in such a way that it prefers the left hand side, if
    * it contains a value, otherwise, it will use the right hand side.
    */
  val tryFallbackTest = test("Try") {
    import scala.util._

    def fallback[A](left: Try[A], right: Try[A]): Try[A] = ???

    assertTrue(fallback(Failure(new Throwable), Success(42)) == Success(42))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Implement `fallback` in such a way that it prefers the left hand side, if
    * it contains a value, otherwise, it will use the right hand side.
    */
  val eitherFallbackTest = test("Either") {
    def fallback[E, A](left: Either[E, A], right: Either[E, A]): Either[E, A] = ???

    assertTrue(fallback(Left("Uh oh!"), Right(42)) == Right(42))
  } @@ ignore

  def exercise =
    suite("Fallbacks")(
      optionFallbackTest,
      tryFallbackTest,
      eitherFallbackTest
    )

}

object FlatMapTests extends Lesson {

  /** ✏ EXERCISE
    *
    * Using `Option##flatMap`, use the `parseInt` helper function to implement a
    * correct `Natural.fromString` constructor, which will succeed only if the
    * string is a number, and if that number is non-negative.
    */
  val optionFlatmapTest = test("Option") {
    def parseInt(i: String): Option[Int] =
      try Some(i.toInt)
      catch { case _: Throwable => None }

    final case class Natural(value: Int)

    object Natural {
      def fromString(value: String): Option[Natural] = {
        parseInt(value)

        ???
      }
    }

    assertTrue(Natural.fromString("123").isDefined)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `Try#flatMap`, use the `parseInt` helper function to implement a
    * correct `Natural.fromString` constructor, which will succeed only if the
    * string is a number, and if that number is non-negative.
    */
  val tryFlatmapTest = test("Try") {
    import scala.util._

    def parseInt(i: String): Try[Int] = Try(i.toInt)

    final case class Natural(value: Int)

    object Natural {
      def fromString(value: String): Try[Natural] = {
        parseInt(value)

        ???
      }
    }

    assertTrue(Natural.fromString("123").isSuccess)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `Either##flatMap`, use the `parseInt` helper function to implement a
    * correct `Natural.fromString` constructor, which will succeed only if the
    * string is a number, and if that number is non-negative.
    */
  val eitherFlatMapTest = test("Either") {
    def parseInt(i: String): Either[String, Int] =
      try Right(i.toInt)
      catch {
        case e: NumberFormatException => Left(e.getMessage())
      }

    final case class Natural(value: Int)

    object Natural {
      def fromString(value: String): Either[String, Natural] = {
        parseInt(value)

        ???
      }
    }

    assertTrue(Natural.fromString("123").isRight)
  } @@ ignore

  def exercise =
    suite("FlatMaps")(
      optionFlatmapTest,
      tryFlatmapTest,
      eitherFlatMapTest
    )

}

object BothExceptions extends Lesson {

  /** ✏ EXERCISE
    *
    * Implement `both` in a way that, when values are present on both sides,
    * will produce a tuple of those values.
    */
  val bothOptionTest = test("Option") {
    def both[A, B](left: Option[A], right: Option[B]): Option[(A, B)] = ???

    assertTrue(both(Some(4), Some(2)) == Some((4, 2)))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Implement `both` in a way that, when values are present on both sides,
    * will produce a tuple of those values.
    */
  val bothTryTest = test("Try") {
    import scala.util._

    def both[A, B](left: Try[A], right: Try[B]): Try[(A, B)] = ???

    assertTrue(both(Try(4), Try(2)) == Try((4, 2)))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Implement `both` in a way that, when values are present on both sides,
    * will produce a tuple of those values.
    */
  val bothEitherTest = test("Either") {
    def both[E, A, B](left: Either[E, A], right: Either[E, B]): Either[E, (A, B)] = ???

    assertTrue(both(Right(4), Right(2)) == Right((4, 2)))
  } @@ ignore

  def exercise =
    suite("Both Exceptions")(
      bothOptionTest,
      bothTryTest,
      bothEitherTest
    )
}

// porting exceptions
object PortingExceptions extends Lesson {

  /** ✏ EXERCISE
    *
    * Using `Option##flatMap`, use the `parseInt` helper function to implement a
    * correct `Natural.fromString` constructor, which will succeed only if the
    * string is a number, and if that number is non-negative.
    */
  val portingOptionTest = test("Option") {
    def parseInt(i: String): Option[Int] =
      try Some(i.toInt)
      catch { case _: Throwable => None }

    final case class Natural(value: Int)

    object Natural {
      def fromString(value: String): Option[Natural] = {
        parseInt(value)

        ???
      }
    }

    assertTrue(Natural.fromString("123").isDefined)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `Try#flatMap`, use the `parseInt` helper function to implement a
    * correct `Natural.fromString` constructor, which will succeed only if the
    * string is a number, and if that number is non-negative.
    */
  val portingTryTest = test("Try") {
    import scala.util._

    def parseInt(i: String): Try[Int] = Try(i.toInt)

    final case class Natural(value: Int)

    object Natural {
      def fromString(value: String): Try[Natural] = {
        parseInt(value)

        ???
      }
    }

    assertTrue(Natural.fromString("123").isSuccess)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `Either##flatMap`, use the `parseInt` helper function to implement a
    * correct `Natural.fromString` constructor, which will succeed only if the
    * string is a number, and if that number is non-negative.
    */
  val portingEitherTest = test("Either") {
    def parseInt(i: String): Either[String, Int] =
      try Right(i.toInt)
      catch {
        case e: NumberFormatException => Left(e.getMessage())
      }

    final case class Natural(value: Int)

    object Natural {
      def fromString(value: String): Either[String, Natural] = {
        parseInt(value)

        ???
      }
    }

    assertTrue(Natural.fromString("123").isRight)
  } @@ ignore

  def exercise =
    suite("Porting Exceptions")(
      portingOptionTest,
      portingTryTest,
      portingEitherTest
    )

}

object MixedExceptions extends Lesson {

  /** ✏ EXERCISE
    *
    * Find a way to combine an Option and a Try in a way that loses no
    * information.
    */
  val optionTryTest = test("Option/Try") {
    import scala.util._

    type User = String
    type Docs = List[String]

    def getUser: Option[User] = Some("sherlock@holmes.com")
    def getDocs: Try[Docs]    = Try(List("Doc 1", "Doc 2"))

    def getUserAndDocs = {
      getUser
      getDocs
      ???
    }

    assertTrue(getUserAndDocs == ???)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Find a way to combine an Either and an Option in a way that loses no
    * information.
    */
  val eitherOptionTest = test("Either/Option") {
    import scala.util._

    type User = String
    type Docs = List[String]

    def getUser: Either[String, User] = Right("sherlock@holmes.com")
    def getDocs: Option[Docs]         = Some(List("Doc 1", "Doc 2"))

    def getUserAndDocs = {
      getUser
      getDocs
      ???
    }

    assertTrue(getUserAndDocs == ???)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Find a way to combine an Either and a Try in a way that loses no
    * information.
    */
  val eitheryTryTest = test("Either/Try") {
    import scala.util._

    type User = String
    type Docs = List[String]

    def getUser: Either[String, User] = Right("sherlock@holmes.com")
    def getDocs: Try[Docs]            = Try(List("Doc 1", "Doc 2"))

    def getUserAndDocs = {
      getUser
      getDocs
      ???
    }

    assertTrue(getUserAndDocs == ???)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Find a way to combine an Either, a Try, and an Option in a way that loses
    * no information.
    */
  val eitherTryOptionTest = test("Either/Try/Option") {
    import scala.util._

    type User  = String
    type Docs  = List[String]
    type Prefs = Map[String, Boolean]

    def getUser: Either[String, User] = Right("sherlock@holmes.com")
    def getDocs: Try[Docs]            = Try(List("Doc 1", "Doc 2"))
    def getPrefs: Option[Prefs]       = Some(Map("autosave" -> true))

    def getUserAndDocsAndPrefs = {
      getUser
      getDocs
      getPrefs
      ???
    }

    assertTrue(getUserAndDocsAndPrefs == ???)
  } @@ ignore

  def exercise =
    suite("Mixed Exceptions")(
      optionTryTest,
      eitherOptionTest,
      eitheryTryTest,
      eitherTryOptionTest
    )
}
