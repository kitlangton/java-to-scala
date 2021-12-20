package course.m1_basics

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**   - Value
  *   - Either[Error, Success]
  *   - Future[List[User]].orElse(Future.succeed(List.empty))
  *   - Success (this might blow up with an exception)
  *
  *   - Implicits
  *   - Exceptions -> Either
  *   - Collections
  */

object Day3_Followup {
  val nullable: String = null

  val optional: Option[String] = None
}

object Extractors extends App {

  final case class Publisher(name: String)

  final case class Book(title: String, pages: Int, publisher: Publisher)

  val publisher  = Publisher("O'Reilly")
  val book       = Book("Scala for the Impatient", 10, publisher)
  val book2      = Book("Rust for the Angry", 10, publisher)
  val theRedBook = Book("Functional Programming in Scala", 358, publisher)
  val mobyDick   = Book("Moby Dick", 10_000, publisher)

  object EvenPages {
    def apply(pages: Int): Int = pages + 1

    def unapply(pages: Int): Option[String] =
      if (pages % 2 == 0) Some(pages.toString + "!!!") else None
  }

  val pages = EvenPages(10)

  book2 match {
    case Book(title, EvenPages(pages), publisher) =>
      println(s"$title by $publisher with even pages $pages")
    case _ => println("No match")
  }
}

object PartialFunctionExample extends App {
  // Total function is defined for all inputs
  // Partial function is undefined for some inputs

  def dividePartial(dividend: Int, divisor: Int): Int =
    dividend / divisor

  println(dividePartial(100, 2))
  println(dividePartial(8, 2))
//  println(dividePartial(8, 0))

  def divideOption(dividend: Int, divisor: Int): Option[Int] =
    if (divisor == 0) None
    else Some(dividend / divisor)

  println(divideOption(100, 2))
  println(divideOption(8, 2))
  println(divideOption(8, 0))

  val dividePartialFunction: PartialFunction[(Int, Int), Int] = {
    case (dividend, divisor) if divisor != 0 => dividend / divisor
  }

  val fallback: PartialFunction[(Int, Int), Int] = { case _ =>
    0
  }

  val result: Int = (100, 0) match {
    case dividePartialFunction(result) => result
    case _                             => 99999
  }

  println(result)

  final case class Person(name: String)

  private val people = List(Person("John"), Person("Mary"))
  val jPeople: List[String] = people.collect {
    case Person(name) if name.startsWith("J") => name
  }

  println(people)
  println(jPeople)

//  println(dividePartialFunction.lift(100, 2))
//  println(dividePartialFunction.lift(8, 2))
//  println(dividePartialFunction.lift(8, 0))

  val divideMyPartialFunction: MyPartialFunction[(Int, Int), Int] =
    MyPartialFunction[(Int, Int), Int] {
      case (dividend, divisor) if divisor != 0 => dividend / divisor
      case _                                   => ???
    }

  println("---")
  println(divideMyPartialFunction.unapply((100, 0)))
  println(divideMyPartialFunction.unapply((100, 2)))

}

trait MyPartialFunction[In, Out] {
  def apply(in: In): Out

  def isDefinedAt(in: In): Boolean

  def unapply(in: In): Option[Out] =
    if (isDefinedAt(in)) Some(apply(in))
    else None
}

object MyPartialFunction {
  def apply[In, Out](f: In => Out): MyPartialFunction[In, Out] =
    new MyPartialFunction[In, Out] {
      override def isDefinedAt(in: In): Boolean =
        try {
          f(in)
          true
        } catch {
          case _: Throwable => false
        }

      override def apply(in: In): Out = f(in)
    }

}

object ImmutableCollections {
  // - Lists
  // - Maps
  // - Sets
  // - Vectors
}
