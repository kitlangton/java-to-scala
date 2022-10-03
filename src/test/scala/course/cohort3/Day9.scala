package course.cohort3

import Fiber.debug
import pprint.pprintln

import java.time.{Duration, Instant}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.language.implicitConversions

object Day9 extends App {
  // Implicit Conversions
  // - add methods to types we do not own
  // - CPS
  // - Custom Future

  // Actual => Expected
  final case class Actual(value: String) {
    def upper: Actual = Actual(value.toUpperCase)
  }
  final case class Expected(value: String) {
    def reverseExpected: Expected = Expected(value.reverse)
  }

  // A => B
  implicit def actualToExpected(actual: Actual): Expected = Expected(actual.value)

  // 1st trigger
  // You're using some type A in the place of B
  // Scala will search for an implicit conversion from A => B
  val actual: Actual     = Actual("actually...")
  val expected: Expected = actual

  // 2st trigger
  // actual.someMethod (someMethod does not exist on Actual)
  // Expected#someMethod (someMethod DOES exist on Expected)
  // convert(actual).someMethod
  val example = actual.reverseExpected

  def spongebobCase(string: String): String =
    string
      .grouped(2)
      .map {
        case str if str.length == 2 => str.take(1).toLowerCase + str.drop(1).toUpperCase
        case other                  => other
      }
      .mkString("")

  implicit class StringExtensions(string: String) {
    def toSpongeBobCase: String = spongebobCase(string)

    def length  = "LENGTH!"
    def lengthh = "LENGTH!"
  }

  val result = "Hello".length

//  implicit def extend(string: String): StringExtensions = StringExtensions(string)

  println(spongebobCase("hello there friends"))
  println("hello there friends".toSpongeBobCase)
  import BooleanSyntax._

  println(true.thirtySeven)

  trait IsSubtypeOf[Sub, Super] extends (Sub => Super) {
    def apply(sub: Sub): Super
  }

  object IsSubtypeOf {
    implicit def example[A]: IsSubtypeOf[A, A] = new IsSubtypeOf[A, A] {
      override def apply(sub: A): A = sub
    }
  }

  final case class Box[A](value: A) {
    def negate(implicit ev: A => Boolean): Box[Boolean] =
      Box(!value)

    def get[B](implicit ev: A <:< Option[B]): Box[B] =
      Box(value.get)
  }

  object Box {
    implicit final class BoxOptionOps[A](private val self: Box[Option[A]]) extends AnyVal {
      def get: Box[A] = Box(self.value.get)
    }
  }

  Box(Option(12)).get

}

object BooleanSyntax {
  implicit final class BooleanOps(private val self: Boolean) extends AnyVal {
    def thirtySeven = 37
  }
}

object CPSExamples extends App {

  def debug(message: String) =
    println(s"${Thread.currentThread().getName}: $message")
  //         Input            Output
  def length(string: String): Int = {
    debug(s"STARTING REG LENGTH $string")
    Thread.sleep(1000)
    string.length
  }

  // Reify Ideas into Values
  // Future
  def async[Out](expr: => Out)(k: Out => Unit): Unit =
    new Thread(() => k(expr)).start()

  val futureInt: (Int => Unit) => Unit = async {
    println("START INT")
    Thread.sleep(100)
    100
  }

  val futureString: (String => Unit) => Unit = async {
    println("START STRING")
    Thread.sleep(100)
    "HELLO"
  }

  val zippedAsync: (((String, Int)) => Unit) => Unit = { k =>
    futureInt { int =>
      futureString { string =>
        k((string, int))
      }
    }
  }

  zippedAsync { case (str, i) =>
    println(s"$str and $i")
  }

  debug("HELLO")
}

// Some running computation that will eventually return A

sealed trait FutureState[A] extends Product with Serializable

object FutureState {
  final case class Executing[A](ks: List[A => Unit]) extends FutureState[A]
  final case class Result[A](value: A)               extends FutureState[A]
}

// Handle on an ongoing computation
class Fiber[A] {

  var state: FutureState[A] = FutureState.Executing(List.empty)

  // technically, this is more of a Promise operation
  def complete(result: A): Unit =
    state match {
      case FutureState.Executing(ks) =>
        state = FutureState.Result(result)
        ks.reverse.foreach(k => k(result))
      case FutureState.Result(_) =>
        throw new Error("UH... already completed. What's the deal?")
    }

  // 1. We have a result!
  // 2. We don't YET have a result...
  def foreach(k: A => Unit): Unit =
    state match {
      case FutureState.Executing(ks) => state = FutureState.Executing(k :: ks)
      case FutureState.Result(a)     => k(a)
    }

  def join: Effect[A] =
    Effect[A](cont => foreach(cont))

  def map[B](f: A => B): Fiber[B] = {
    val bFuture = new Fiber[B]
    foreach(a => bFuture.complete(f(a)))
    bFuture
  }

  def flatMap[B](f: A => Fiber[B]): Fiber[B] = {
    val bFuture = new Fiber[B]
    foreach { a =>
      f(a).foreach { b =>
        bFuture.complete(b)
      }
    }
    bFuture
  }

  def timed: Fiber[(Duration, A)] = {
    val timedFuture = new Fiber[(Duration, A)]
    val start       = Instant.now
    foreach { a =>
      val diff = Duration.between(start, Instant.now)
      timedFuture.complete((diff, a))
    }
    timedFuture
  }

  def zip[B](that: => Fiber[B]): Fiber[(A, B)] = {
    val abFuture = new Fiber[(A, B)]
    foreach { a =>
      that.foreach { b =>
        val result: (A, B) = (a, b)
        abFuture.complete(result)
      }
    }
    abFuture
  }

}

object Fiber {

  def debug(message: String): Unit =
    println(s"${Thread.currentThread().getName}: $message")

  def apply[A](expr: => A)(implicit ec: ExecutionContext): Fiber[A] = {
    val myFuture = new Fiber[A]
    ec.execute { () =>
      val result = expr
      myFuture.complete(result)
    }
    myFuture
  }
}

object MyFutureExamples extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  // convert functions into EFFECT
  def delayed[A](expr: => A): Fiber[A] = Fiber {
    val result       = expr
    val startingTime = Instant.now()
    debug(s"START $result")
    Thread.sleep(1000)
    val duration = Duration.between(startingTime, Instant.now())
    debug(s"DONE $result TOOK $duration")
    result
  }

  val x = "hello"
  val y = 123
  x.length + y

  // not "referentially transparent"
  // Future = handle, reference to an ongoing computation
  // Effect = Blueprints. Descriptions of computations
  val intFuture = delayed(123)
//  for {
//    x <- delayed("hello")
//    y <- intFuture
//  } yield x.length + y

  Thread.sleep(4000)
}
