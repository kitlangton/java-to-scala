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
  val password = s"$name-5000!!!"
}

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
object Disconnected {
  println("Hello?  World?")
}

/** ✏ EXERCISE
  *
  * Define a method that takes a String and returns its length (add a default)
  */
object MethodDefinitions {
  // ???
}

/** ✏ EXERCISE
  *
  * How should we "idiomatically" use objects?
  */

class Food(string: String)

object Food {
  // What should go in here?
}

object MathUtils {
  // What should go in here?
}

/** ✏ EXERCISE
  *
  * Defining values
  */
object DefiningValues {
  // 1. Create a value that we cannot modify

  // 2. Create a value that we can modify
}

/** ✏ EXERCISE
  *
  * Refactor the following to take advantage of the fact that if/else is an
  * expression in Scala.
  */
object ExpressionVsStatement extends App {
  val input = Random.nextInt(100)

  // Refactor this part to use power of expressions
  // --------------------
  var result: String = ""

  if (input > 80) {
    result = "TOO BIG TO TELL"
  } else if (input % 2 == 0) {
    result = "EVEN"
  } else {
    result = "ODD"
  }
  // --------------------

  println(s"$input is $result")
}

/** ✏ EXERCISE
  *
  * Rewrite `greeting` to use String interpolation!
  */
object StringInterpolation extends App {

  val name = "Bob"
  val age  = 42

  val greeting: String =
    "Hello, my name is " + name + " and I am " + age + " years old."

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

  println(ifElseString(false, "YES!", "NO!"))
  println(ifElseInt(false, 1, 0))
}

/** A FEW MORE POINTS
  *
  *   - Polymorphic methods
  *   - By-Name parameters
  *   - Lazy Vals
  */
