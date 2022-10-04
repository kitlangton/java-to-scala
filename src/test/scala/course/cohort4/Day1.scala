package course.cohort4

import scala.reflect.internal.settings.MutableSettings

// Welcome!

final case class Number(
    value: Int
) {
  def addOne: Number = Number(value + 1)
}

class MutableNumber(var value: Int) {
  def addOne(): Unit = value += 1

  override def toString: String = s"MutableNumber($value)"
}

object ValVsVar extends App {
  val nums = (1 to 10).toList

  private val zero = Number(0)

  val result = nums.foldLeft(zero) { (number, _) =>
    println(s"adding 1 to $number")
    number.addOne
  }

  println(s"Zero: $zero")
  println(s"Result: $result")

  val mutableNumber = new MutableNumber(0)
  val zero2         = mutableNumber
  (1 to 10).foreach { _ =>
    println(s"adding 1 to $mutableNumber")
    mutableNumber.addOne()
  }

  println(s"mutable number is $zero2")
  println(s"mutable number is $mutableNumber")

//  println(s"result: $result")
//  var i      = 0
//  var number = Number(0)
//
//  while (i < 10) {
//    number = number.addOne
//    i += 1
//  }
}

//def addSomething(x: Int, y: Int): Int = x + y

object MethodExample extends App {
  def add(string: String, y: Int): Int =
    string.length + y

  // Statements to not evaluate to a result

  // Invalid syntax
  // cmd-p
  val result = add("hello", 10)

  println(result)
}

object Expressionsssss extends App {
  // In Scala, most terms are expressions

  def check(x: Int): String =
    if (x > 10) "It's very true"
    else if (x > 5) "It's true"
    else "It's false"

  println(check(10))
  println(check(5))
}

// private & immutable (private val)
class Person(val name: String, age: Int) {
  def introduction: String = s"My name is $name. I'm $age years old"

//  def becomeKyle(): Unit =
//    name = "Kyle"
}

// singleton
object PersonExamples extends App {
  val kit = new Person("Kit", 32)
  println(kit.name)
//  kit.becomeKyle()
  println(kit.name)
}

object Math {
  var favoriteNumber: Int = 0

  def isFavorite(n: Int): Boolean =
    n == favoriteNumber
}

object Something extends App {
  Math.favoriteNumber = 100
  println(Math.isFavorite(100)) // true
}
//println(Math.max(1, 100)) // 100
//

class Coord(val x: Int, val y: Int) {
  def z: Int = 123

  def flip: Coord =
    new Coord(x = y, y = x)

  def midpoint(that: Coord): Coord =
    new Coord(
      x = (this.x + that.x) / 2,
      y = (this.y + that.y) / 2
    )

  def center(coord2: Coord, coord3: Coord): Coord =
    new Coord(
      x = (this.x + coord2.x + coord3.x) / 3,
      y = (this.y + coord2.y + coord3.y) / 3
    )

  override def toString: String = s"Coord($x, $y)"
}

object Coord {
  // Constructors!
  def zero: Coord               = new Coord(0, 0)
  def horizontal(x: Int): Coord = new Coord(x, 0)
  def vertical(y: Int): Coord   = new Coord(0, y)

  // unary methods
  def flip(coord: Coord): Coord =
    new Coord(x = coord.y, y = coord.x)

  // binary methods
  def midpoint(coord1: Coord, coord2: Coord): Coord =
    new Coord(
      x = (coord1.x + coord2.x) / 2,
      y = (coord1.y + coord2.y) / 2
    )

  def center(coord1: Coord, coord2: Coord, coord3: Coord): Coord =
    new Coord(
      x = (coord1.x + coord2.x + coord3.x) / 3,
      y = (coord1.y + coord2.y + coord3.y) / 3
    )

  def apply(string: String, y: Int): Coord = {
    val coord3 = new Coord(5, 10)
    coord3
  }
//    new Coord(string.length, y)
}

object CoordDemo extends App {
  val coord  = new Coord(10, 20)
  val coord2 = new Coord(10, 20)
  println(coord)
  println(Coord.flip(coord))
  println(coord.flip)

  println(Coord.midpoint(coord, coord2))
  // 10 + 5
  // infix notation
//  println(10.+(5) + 100 + 10)
  // ((SomeType + SomeType) + SomeType) + SomeType
  // (SomeType)

  println(coord.midpoint(coord2))
  println(coord midpoint coord2 midpoint coord)

//  println(Coord.center(coord, coord2, coord2))
//  println(coord2 center (coord, coord2))
}

object Something2 extends App {
  val huh: String = "hello"
  println(huh.length)

  // arity
  def nullary() = 0

  // unary
  def unary(int: Int)       = 1
  def square(int: Int): Int = int * int

  // binary
  // 2
  def add(x: Int, y: Int): Int = x + y

  // arity = 3
  // ternary
//  def ternary(condition: Boolean, trueCase: ???, falseCase: ???): ??? = ???
  def add3(x: Int, y: Int, z: Int): Int = x + y + z
}

trait Organism {
  def genus: String
  def species: String

  def binomialNomenclature: String = s"$genus $species"
}

object OrganismDemo extends App {
  val malaysianFungus: Organism =
    new Organism {
      override def genus: String   = "spongebob"
      override def species: String = "squarepantsii"
    }

  println(malaysianFungus.genus) // Spongiforma
}

class Friend {
  def name: String           = "Kit"
  private def secret: String = "I'm a secret"
}

// 1. companion objects can access private members
// 2. w/r/t implicits
object Friend {
  def greet(friend: Friend): String =
    s"Hello, ${friend.name}. Do you remember that thing you told me not to tell anyone? " +
      s"You said: ${friend.secret}!!!"
}

object FriendDemo extends App {
  val friend = new Friend
  println(Friend.greet(friend))
}

class Dog(val name: String) extends Organism {
  def genus: String   = "Canis"
  def species: String = "familiaris"
}

object Experiment extends App {

  def identify(organism: Organism): Unit =
    println(s"This looks to me like a ${organism.genus} ${organism.species}!")

  // Dog is a subtype of Organism
  // Dog <: Organism
  // -> -> ->  widening
  // a subtype contains all of the information of the supertype
  val sparky: Organism = new Dog("Sparky")
//  val organismy: Organism = new Organism {
//    override def genus: String   = "genusy"
//    override def species: String = "speciesy"
//  }
//  println(organismy.name)
  identify(sparky) // This looks to me like a Canis familiaris!

  def possible(unit: Unit) = 100

  def impossible(nothing: Nothing) = 100

  // cannot inherit multiple abstract classes
  //
  // modeling data
  // I use sealed traits and case classes
  // ADTs
  // trait UserRepository
  // case class UserRepositoryLive() extends UserRepository

  // :: <- association
  // Nothing == 0
  // Nothing <: EverythingElse
  val int: Int = ???

  impossible(???)
  possible(())
}
