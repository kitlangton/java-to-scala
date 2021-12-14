package course.m1_basics

import scala.util.Random

/** ===Follow Up===
  *
  *   - `null.asInstanceOf[Int]`
  *   - Special Companion Object properties!
  */

object CastingNull extends App {
  println(null.asInstanceOf[Boolean])
  println(null.asInstanceOf[Int])
  println(null.asInstanceOf[Double])
  println(null.asInstanceOf[Char])
}

class Blob(name: String) {
  private val password = s"$name-5000!!!"
}

// implicits
// Blob companion object can access private members of the Blob class
object Blob extends App {
  val defaultBlob = new Blob("Billy")

  println(defaultBlob.password)
}

/** ✏ EXERCISE
  *
  * How can we make this object runnable? There are two different ways to do
  * this.
  */
object Disconnected extends App {
  println("Hello?  World?")
}

/** ✏ EXERCISE
  *
  * Define a method that takes a String and returns its length (add a default)
  */
object MethodDefinitions {
  def length(x: Int, s: String = "DEFAULT VALUE"): Int = s.length

  def main(args: Array[String]): Unit =
    println(length(12))
}

/** ✏ EXERCISE
  *
  * How should we "idiomatically" use objects?
  */

class Food(string: String)

object Food {
  // What should go in here?

  // CONSTANTS

  val Apple  = new Food("Apple")
  val Banana = new Food("Banana")

  // CONSTRUCTORS
  def fromInt(x: Int): Food =
    x match {
      case 0 => Apple
      case 1 => Banana
      case _ => new Food("SOYLENT")
    }
}

object MathUtils {
  val PI = 3.141592653589793

  // What should go in here?
  def add(x: Int, y: Int): Int =
    x + y
}

/** ✏ EXERCISE
  *
  * Defining values
  */
object DefiningValues {
  // 1. Create a value that we cannot modify
  val immutable = "This is immutable"
  // immutable = "I AM A BANANA" - NO!

  // 2. Create a value that we can modify
  var mutable = "This is mutable"
  mutable = "I AM A BANANA"
}

/** ✏ EXERCISE
  *
  * Refactor the following to take advantage of the fact that if/else is an
  * expression in Scala.
  */
object ExpressionVsStatement extends App {
  val input: Int = Random.nextInt(100) // 0 - 99

  // Refactor this part to use power of expressions
  // --------------------
  val result: String =
    if (input > 80) "TOO BIG TO TELL"
    else if (input % 2 == 0) "EVEN"
    else "ODD"
  // --------------------

  println(s"$input is $result")
}

/** ✏ EXERCISE
  *
  * Rewrite `greeting` to use String interpolation!
  */
object StringInterpolation extends App {

  val name = "Bob"
  val age  = 42.5558123

  val greeting: String =
    s"Hello, my name is $name and I am ${age + 100} years old."

  println(greeting)
}

/** ✏ EXERCISE
  *
  * Below are two custom, type-specific if/else methods. There is some
  * duplication that we'd like to remove. Create an `ifElse` method that works
  * for any type.
  */
object PolymorphicMethods extends App {
  def ifElseString(condition: Boolean, onTrue: String, onFalse: String): String =
    if (condition) onTrue else onFalse

  def ifElseInt(condition: Boolean, onTrue: Int, onFalse: Int): Int =
    if (condition) onTrue else onFalse

  def ifElse[A](condition: Boolean, onTrue: A, onFalse: A): A =
    if (condition) onTrue else onFalse

  println(ifElse(true, "YES!", "NO!"))

  println(ifElse(true, 1, 0))
}

object BeTheComputer {
  // Reduce expressions one step at a time
  def adder(amount: Int): Int => Int =
    x => x + amount

  val result: Int =
    adder(10)(15) // -> 25

  val result1: Int =
    ((x: Int) => x + 10)(15) // -> 25

  val result2: Int =
    15 + 10 // -> 25

  val result3: Int =
    25 // -> 25
}

object ExampleCurrying extends App {
  def addNormal(x: Int, y: Int): Unit =
    println(x + y)

  def add(x: Int)(y: Int): Unit =
    println(x + y)

  val arrayOfNumbers: Array[Int] =
    Array(1, 2, 3, 4, 5)

  arrayOfNumbers.foreach(x => x + 10)

  val x1: Int => Int =
    x => x + 10

  val x1__ : Function1[Int, Int] =
    new Function1[Int, Int] {
      override def apply(v1: Int): Int = v1 + 10
    }

  val another: (Int, String) => Boolean =
    (x, y) => true

  // arity
  val another__ : Function2[Int, String, Boolean] =
    new Function2[Int, String, Boolean] {
      override def apply(v1: Int, v2: String): Boolean = true
    }

  val result: Boolean =
    another__(1, "hello")

  object SillyThing extends (Int => String) {
    def apply(int: Int): String =
      "Silly thing"
  }

  def needsThing(f: Int => String): Unit =
    println(f(1))

  val result2: String =
    SillyThing(12)

  val x1_ : Int => Int =
    _ + 10

  val x2: (Int, String) => Int =
    (x, str) => str.length + x + 10

  val x2_ : (Int, String) => Int =
    _ + _.length + 10

  val x2__ : (Int, String) => Int =
    (x, y) => x + y.length + 10

  // types as sets of values
  // Unit -> ()
  // Boolean -> true, false
}

/** A FEW MORE POINTS
  *
  *   - Polymorphic methods
  *   - By-Name parameters
  *   - Lazy Vals
  */
