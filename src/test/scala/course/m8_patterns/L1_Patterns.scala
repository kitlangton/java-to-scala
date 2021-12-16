package course.m8_patterns

import course.Lesson.???
import course.m8_patterns.StringUtils._

/** A large part of functional programming involves seeking out powerful
  * abstractions. Sometimes this involves squinting just a little bit harder.
  * These "programming" patterns tend to be more abstract than their object
  * oriented brethren. This can make them both more generally applicable while
  * harder to understand. Let's break down this process by exploring some common
  * patterns.
  */
object Combining extends App {

  // What's the pattern?
  val add    = 4 + 8
  val mult   = 2 * 4
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
  example("Hello", " ", "world!")(_ + _, "+")
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
  *   - And there exists an identity element `empty` where`
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
  }

  /** ✏ EXERCISE
    *
    * As a first stab. Let's create a case class, just a bag of methods
    * describing what we want to achieve.
    */

  type Monoid = ???
}

object UsingTheMonoid extends App {

  final case class Student(name: String)

  final case class Subject(name: String)

  val classAssignments1 =
    Map(
      Subject("Math")    -> Set(Student("John"), Student("Mary")),
      Subject("English") -> Set(Student("John"), Student("Mary"), Student("Shishu"))
    )

  val classAssignments2 =
    Map(
      Subject("Math")    -> Set(Student("George"), Student("Mary"), Student("Jorge")),
      Subject("English") -> Set(Student("Peter"), Student("Mary"), Student("Fitzpatrick"))
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

  // implement a method that merges maps of sets
  def merge[A, B](map1: Map[A, Set[B]], map2: Map[A, Set[B]]): Map[A, Set[B]] =
    map2.foldLeft(map1) { case (acc, (k, v)) =>
      acc.updated(k, acc.getOrElse(k, Set()) ++ v)
    }

//  println("\nRECURSIVELY MERGED CLASS ASSIGNMENTS")
//  println(merge(classAssignments1, classAssignments2).mkString("\n"))

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

  val result1 = ???

  // 2.
  // Ingredients
  // -----------
  val someA: A = ???
  val someB: B = ???

  val result2 = ???
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

  final case class Vessel[A](value: A) {
    def map[B](f: A => B): Vessel[B] =
      Vessel(f(value))

    def zip[B](that: Vessel[B]): Vessel[(A, B)] =
      Vessel((value, that.value))
  }

  val stringVessel: Vessel[String] = Vessel("What is the meaning of life?")
  val mappedVessel: Vessel[Int]    = stringVessel.map(_.length)

  val intVessel: Vessel[Int]      = Vessel(42)
  val boolVessel: Vessel[Boolean] = Vessel(true)

  val zippedVessel: Vessel[(Int, Boolean)] =
    intVessel.zip(boolVessel)

  // - Explore the same concept with a Producer

}

object Generalizing {
  // 1. Create a type class for structures that can be mapped

  // 2. Create a type class for structures that can be zipped
}
