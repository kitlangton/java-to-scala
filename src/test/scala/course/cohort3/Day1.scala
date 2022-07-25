package course.cohort3

import pprint.pprintln

// how to generate Java code from Scala
//object Math {
//  var x = 0
//  println("MATH IS ALIVE!")
//  def max(a: Int, b: Int): Int = if (a >= b) a else b
//  def min(a: Int, b: Int): Int = if (a <= b) a else b
//  def inc                      = x += 1
//}

// 1. Be a grab bag of utility methods, values, constants
// example: Math, StringUtils

// HttpService
// new HttpService(...)
// HttpService.
// HttpServiceFactor.build
// new HttpServiceBuilder()
class Coord(val x: Int, val y: Int) {
  private val secretMessage = "I am a secret"

  def midpoint(that: Coord): Coord =
    new Coord((x + that.x) / 2, (y + that.y) / 2)

  def midpoint(that: List[Coord]): Coord =
    ???

  def leftmostNeighbor: Coord = new Coord(x - 1, y)

  override def toString = "(" + x + "," + y + ")"
}

// SECRET COMPANION ABILITIES
// 1. private members
// 2. consequences w/r/t implicit resolution
object Coord {
  // constructor
  // all the ways of building that type.
  def zero               = new Coord(0, 0)
  def horizontal(x: Int) = new Coord(x, 0)
  def vertical(y: Int)   = new Coord(0, y)

  def getSecret(coord: Coord): String = coord.secretMessage

  // methods that operate on that type, which do not necessarily belong on the Class itself
  // combinator
  def leftmostNeighbor(c: Coord): Coord     = new Coord(c.x - 1, c.y)
  def midpoint(c1: Coord, c2: Coord): Coord = ???
  def midpoint(coords: List[Coord]): Coord  = ???
}

object CoordExample extends App {
  // celestial domain of types
  // terrestrial world of terms
  // type namespace vs term namespace
  // Coord class       Coord companion
  val c1: Coord = new Coord(1, 2)
  val c2: Coord = new Coord(3, 4)
  val c3: Coord = new Coord(5, 6)
  // unary belong on the class
  pprintln(c1.leftmostNeighbor) // √
  12
  "hello"
  val Name = "Kit"

  class Thing(name: String)
  val Thing: Thing = new Thing("Kit")

  object MathUtils

  val mathUtilsType: MathUtils.type = MathUtils

  val whatIsTheTypeOfTheCompanionObject: Coord.type =
    Coord

  pprintln(Coord.leftmostNeighbor(c1)) // X

  // binary methods belong on the class
  pprintln(c1.midpoint(c2))        // √
  pprintln(Coord.midpoint(c1, c2)) // X

  // greater than 2 arity methods these belong on the companion
  pprintln(Coord.midpoint(List(c1, c2, c3))) // √
  pprintln(c1.midpoint(List(c2, c3)))
}

object StringUtils {
  def toCamelCase(str: String): String = {
    val words     = str.split(" ")
    val camelCase = words.map(word => word.head.toUpper + word.tail).mkString
    camelCase
  }
}

object TestSomeCode extends App {
  println(Math.abs(-100))
}

object Day1 extends App {

  class Person(val name: String, age: Int) {
    def introduction: String = s"My name is $name. I'm $age years old"
  }

  val balthazar = new Person("Balthazar", 55)

  class Box(val items: List[Int])

  val myBox   = new Box(List(1, 2, 3, 4, 5))
  val myList  = myBox.items
  val newList = myList.appended(10)

  pprintln(myBox.items)
  println(newList)

}

object FunctionLiterals extends App {
  // INSTRUCTIONS
  // ============
  // Create a function/lambda which squares its argument.

//  val square: Int => Int = ???
//
//  val result = square(10)
//  assert(result == 100, s"Expected 100, got $result")

  val plus: (Int, Int) => Int = ???

  val result = plus(10, 20)
  assert(result == 30, s"Expected 30, got $result")

}

object FunctionLiteral extends App {
  // INSTRUCTIONS
  // ============
  // Create a function/lambda which squares its argument.

  val square: Int => Int = ???

  val result = square(10)
  assert(result == 100, s"Expected 100, got $result")

}
