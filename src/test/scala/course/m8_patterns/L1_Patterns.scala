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

/** Other patterns:
  *   - Types with a `map` method
  *   - Types with a `flatMap` method
  *   - Types with a `zip` method
  */
