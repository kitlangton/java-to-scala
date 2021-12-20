package course.m8_patterns

import course.Lesson.???
import course.m8_patterns.ReifyingThePattern.{Monoid, collapse}
import course.m8_patterns.StringUtils._

import scala.annotation.implicitNotFound
import scala.util.control.NonFatal

/** A large part of functional programming involves seeking out powerful
  * abstractions. Sometimes this involves squinting just a little bit harder.
  * These "programming" patterns tend to be more abstract than their object
  * oriented brethren. This can make them both more generally applicable while
  * harder to understand. Let's break down this process by exploring some common
  * patterns.
  */
object Combining extends App {

  // What's the pattern?
  val add    = 4 + 8 + 10
  val mult   = 2 * 4 * 123
  val string = "Hello, " + "world!"
  val list   = List(1, 2, 3) ++ List(4, 5, 6)
  val and    = true && false
  val or     = false || true
  val map    = Map(1 -> "One", 2 -> "Two") ++ Map(3 -> "Three", 4 -> "Four")

  println(map)
}

object CombiningProperty1 extends App {
  def example[A](a1: A, a2: A, a3: A)(op: (A, A) => A, opName: String): Unit = {
    println(
      s"\n(${blue(a1)} ${green(opName)} ${blue(a2)}) ${green(opName)} ${blue(a3)} = ${blue(op(op(a1, a2), a3))}"
    )
    // right associative
    println(
      s"${blue(a1)} ${green(opName)} (${blue(a2)} ${green(opName)} ${blue(a3)}) = ${blue(op(a1, op(a2, a3)))}"
    )
  }

  example(1, 2, 3)(_ + _, "+")
  example(1, 2, 3)(_ * _, "*")
  example("Hello", "There", "world!")(_ + _, "+")
  example(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))(_ ++ _, "++")
  example(true, true, false)(_ && _, "&&")
  example(true, false, true)(_ || _, "||")
  example(Map(1 -> "One"), Map(2 -> "Two"), Map(3 -> "Three"))(_ ++ _, "++")
}

object CombiningProperty2 extends App {

  val add    = 1 + 0
  val mult   = 2 * 1
  val string = "Hello, " + ""
  val list   = List(1, 2, 3) ++ List()
  val and    = true && true
  val or     = true || false
  val map    = Map(1 -> "One", 2 -> "Two") ++ Map()

  def example[A](value: A)(op: (A, A) => A, opName: String, identity: A): Unit = {
    val combined = op(value, identity)

    println(s"\n${blue(value)} ${green(opName)} ${red(identity)} = ${blue(combined)}")
  }

  example(15)(_ + _, "+", 0)
  example(15)(_ * _, "*", 1)
  example("Hello")(_ + _, "+", "")
  example(List(1, 2, 3))(_ ++ _, "++", List())
  example(true)(_ && _, "&&", true)
  example(true)(_ || _, "||", false)
  example(Map(1 -> "One", 2 -> "Two"))(_ ++ _, "++", Map())
}

/** So we have discovered a pattern
  *   - Data type `A` with associated binary operation `op: (A, A) => A`
  *   - Where `op` is associative
  *   - And there exists an identity element `empty` where
  *     - `op(empty, x) == x` for all `x`
  *
  * This pattern, it turns out has a special name: Monoid!
  *
  * The question is, how do we use this to our benefit
  */
object ReifyingThePattern extends App {

  /** Unfortunately, a normal trait doesn't quite make sense:
    *
    *   - We can't extend the built in types like `Int`, `String`, etc.
    *   - We can't define a static `empty` value
    *   - We can't combine with just any other `Combinable`, but only with
    *     values of the same type (this is fixable with a trait, but awkward)
    */
  trait Combinable {
    def combine(other: Combinable): Combinable

    val empty: Combinable
  }

  /** ✏ EXERCISE
    *
    * As a first stab. Let's create a case class, just a bag of methods
    * describing what we want to achieve.
    */

  @implicitNotFound("No MONOID found for ${A}")
  trait Monoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
  }

  object Monoid {
    implicit val additionMonoid: Monoid[Int] =
      new Monoid[Int] {
        def combine(a1: Int, a2: Int): Int = a1 + a2
        def empty: Int                     = 0
      }

    implicit def setMonoid[A]: Monoid[Set[A]] =
      new Monoid[Set[A]] {
        def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 ++ a2
        def empty: Set[A]                           = Set()
      }

    implicit val stringMonoid: Monoid[String] =
      new Monoid[String] {
        def combine(a1: String, a2: String): String = a1 + a2
        def empty: String                           = ""
      }

    implicit def listMonoid[A]: Monoid[List[A]] =
      new Monoid[List[A]] {
        def combine(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
        def empty: List[A]                             = List()
      }

    implicit def mapMonoid[K, V](implicit valueMonoid: Monoid[V]): Monoid[Map[K, V]] =
      new Monoid[Map[K, V]] {
        def combine(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
          a1.foldLeft(a2) { case (acc, (k, v)) =>
            acc.updated(k, valueMonoid.combine(acc.getOrElse(k, valueMonoid.empty), v))
          }

        def empty: Map[K, V] = Map()
      }

    implicit class MonoidOps[A](val self: A) {
      // List(1, 2, 3) +++ List(4, 5, 6) = List(1, 2, 3, 4, 5, 6)
      def +++(that: A)(implicit monoid: Monoid[A]): A = monoid.combine(self, that)
    }
  }

  final case class Grades(list: List[Int])

  object Grades {
    implicit val gradesMonoid: Monoid[Grades] =
      new Monoid[Grades] {
        def combine(a1: Grades, a2: Grades): Grades = Grades(a1.list ++ a2.list)
        def empty: Grades                           = Grades(List())
      }
  }

  def collapse[A](as: List[A])(implicit monoid: Monoid[A]): A =
    as.foldLeft(monoid.empty)(monoid.combine)

  val listOfInts  = List(1, 10, 15)
  val listOfLists = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
  val listOfListOfStrings = List(
    List("a", "b", "c"),
    List("d", "e", "f"),
    List("g", "h", "i")
  )

  val listOfStrings = List("a", "b", "c")

  val listOfMaps = List(
    Map(1 -> "One", 6   -> "SIIX"),
    Map(3 -> "Three", 4 -> "Four"),
    Map(5 -> "Five", 6  -> "Six")
  )

  val listOfGrades = List(Grades(List(1, 2, 3)), Grades(List(4, 5, 6)))

  println(collapse(listOfGrades))
}

object UsingTheMonoid extends App {

  final case class Student(name: String)

  final case class Subject(name: String)

  val classAssignments1 =
    Map(
      Subject("Math")    -> 10,
      Subject("English") -> 20
    )

  val classAssignments2 =
    Map(
      Subject("Math")    -> 30,
      Subject("English") -> 40
    )

  val combined = classAssignments1 ++ classAssignments2

  println("BROKEN MERGED CLASS ASSIGNMENTS")
  println(combined.mkString("\n"))

  // ======
  // ======
  // ======
  // ======
  // PART 2
  // ======
  // ======
  // ======
  // ======

  // cats
  // zio-prelude

  import course.m8_patterns.ReifyingThePattern.Monoid._
  println("\nRECURSIVELY MERGED CLASS ASSIGNMENTS")

  final case class Turkey(name: String)
  val t1 = Map(1 -> Turkey("Steve"))
  val t2 = Map(2 -> Turkey("Bob"))

//  t1 +++ t2

  private val merged: Map[Subject, Int] =
    classAssignments1 +++ classAssignments2

  println(merged.mkString("\n"))

  // ======
  // ======
  // ======
  // ======
  // PART 3
  // ======
  // ======
  // ======
  // ======

  final case class School(name: String)

  val school1 =
    Map(
      School("St. Peters") -> classAssignments1
    )

  val school2 =
    Map(
      School("St. Peters") -> classAssignments2
    )

  val mergedSchool = school1 ++ school2
//  println("\nMERGED SCHOOLS")
//  println(mergedSchool.mkString("\n"))

  // How do we deal with this!?
}

/** Other patterns:
  *   - Types with a `map` method
  *   - Types with a `flatMap` method
  *   - Types with a `zip` method
  */

/** There are a set of common patterns, each with a fancier name than the last,
  * which all deal with data types that either contain or produce values. Let's
  * explore these producer patterns.
  *
  * But lets start by building some basic intuitions. Instead of thinking about
  * some container of some type A—what can we do with type variables in general?
  */
object Parametricity extends App {

  type A
  type B

  // 1.
  // Ingredients
  // -----------
  val a1: A        = ???
  val aToB: A => B = ???

  val result1: B = aToB(a1)

  // 2.
  // Ingredients
  // -----------
  val someA: A = ???
  val someB: B = ???

  val result2: (A, B) = (someA, someB)

//  val result3: Either[A, B] = Right(someB)
}

object Producers {

  final case class Box[+A](value: A)

  /** 1.
    *
    * If we have a container/producer of A, we can always just extract the value
    * and then call a function on it, transforming it to a B.
    */

  // A
  val stringBox: Box[String] = Box("What is the meaning of life?")

  // A => B
  val stringToInt: String => Int = _.length

  // B
  val int: Int = stringToInt(stringBox.value)

  /** 2.
    *
    * Likewise, if we have two containers/producers of A and B, we can always
    * pluck out each value and then combine them.
    */

  // A
  val intBox: Box[Int] = Box(42)
  // B
  val boolBox: Box[Boolean] = Box(true)

  // (A, B)
  val intAndBoolean: (Int, Boolean) = (intBox.value, boolBox.value)

  /** So, instead of unpacking this container. Why not just transform the
    * container without unpacking/repacking it?
    */

  // ANY DATA STRUCTURE THAT CONTAINS/PRODUCES A GENERIC
  final case class Vessel[A](value: A) {
    def map[B](f: A => B): Vessel[B] =
      Vessel(f(value))

    def zip[B](that: Vessel[B]): Vessel[(A, B)] =
      Vessel((value, that.value))

    def flatten[B](implicit ev: A <:< Vessel[B]): Vessel[B] =
      ev(value)

    def flatMap[B](f: A => Vessel[B]): Vessel[B] =
      f(value)
  }

  val stringVessel: Vessel[String] = Vessel("What is the meaning of life?")
  val mappedVessel: Vessel[Int]    = stringVessel.map(_.length)

  val intVessel: Vessel[Int]      = Vessel(42)
  val boolVessel: Vessel[Boolean] = Vessel(true)

  val zippedVessel: Vessel[(Int, Boolean)] =
    intVessel.zip(boolVessel)

  // - Explore the same concept with a Producer

  // orElse
  def orElse[A1 >: A](that: => FailBox[A1]): FailBox[A1] =
    orElseEither(that).map(_.merge)

  def orElseEither[B](that: FailBox[B]): FailBox[Either[A, B]] =
    try FailBox(() => Left(value()))
    catch {
      case NonFatal(e) =>
        that.map(Right(_))
    }
}

object Generalizing {
  // 1. Create a type class for structures that can be mapped

  // 2. Create a type class for structures that can be zipped
}
