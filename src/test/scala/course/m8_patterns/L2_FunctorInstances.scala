package course.m8_patterns

import course.Lesson
import zio.test.TestAspect.ignore
import zio.test.assertTrue

object L2_FunctorInstances extends Lesson {

  trait Functor[F[+_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  def functorMap[F[+_], A, B](fa: F[A])(f: A => B)(implicit functor: Functor[F]): F[B] =
    functor.map(fa)(f)

  /** ✏ EXERCISE
    *
    * Create a functor for the `List` type, which will map over Lists.
    */

  // CREATE THIS INSTANCE
  implicit lazy val listFunctor: Functor[List] = ???

  val listFunctorTest =
    test("List Functor") {
      val result = functorMap(List(1, 2, 3, 4))(_ + 1)

      assertTrue(result == List(2, 3, 4, 5))
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Create a functor for the `Option` type, which will map over Option's
    * value.
    */

  // CREATE THIS INSTANCE
  implicit lazy val optionFunctor: Functor[Option] = ???

  val optionFunctorTest =
    test("Option Functor") {
      val result = functorMap(Option("Hello"))(_.toUpperCase)

      assertTrue(result == Option("HELLO"))
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Create a functor for the `Either` type, which will map over its `A` value.
    */

  // CREATE THIS INSTANCE
  implicit def eitherFunctor[E]: Functor[Either[E, +*]] =
    new Functor[Either[E, +*]] {
      def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] =
        ???
    }

  val eitherFunctorTest =
    test("Either Functor") {
      val either: Either[Int, String] = Right("Hello")
      val result                      = functorMap(either)(_.toUpperCase)

      assertTrue(result == Right("Hello"))
    } @@ ignore

  /** ✏ EXERCISE
    *
    * CRAZY ADVANCED
    *
    * Create a functor for the `Box` type, which will map over the values,
    * provided those have their own Functor instances.
    */

  final case class Box[F[+_], +A](content: F[A])

  // CREATE THIS INSTANCE
  implicit def boxFunctor[F[+_]](implicit functor: Functor[F]): Functor[Box[F, +*]] =
    new Functor[Box[F, +*]] {
      def map[A, B](fa: Box[F, A])(f: A => B): Box[F, B] =
        Box(functor.map(fa.content)(f))
    }

  val boxFunctorTest1 =
    test("Box Functor 1") {
      val result = functorMap(Box(List(1, 2, 3, 4)))(_ + 1)

      assertTrue(result == Box(List(2, 3, 4, 5)))
    } @@ ignore

  val boxFunctorTest2 =
    test("Box Functor 2") {
      val result = functorMap(Box(Option("Hello")))(_.toUpperCase)

      assertTrue(result == Box(Option("HELLO")))
    } @@ ignore

  override def exercise =
    suite("Functor")(
      listFunctorTest,
      optionFunctorTest,
      eitherFunctorTest,
      boxFunctorTest1,
      boxFunctorTest2
    )
}
