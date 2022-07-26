package course.cohort3

import pprint.pprintln

object ClassPropertyPrivacy extends App {
  // INSTRUCTIONS
  // ============
  // Make the following code compile by changing only the class definition.

  class Person(val name: String, var age: Int) {
    val cool   = true
    var friend = "Bob"
  }

  val balthazar = new Person("Balthazar", 42)
  println(balthazar.name) // oops
  balthazar.age = 1000    // oops again

}

object StatementVsExpression extends App {
  // INSTRUCTIONS
  // ============
  // Refactor the following code to take advantage of the fact that if/else is an expression in Scala.

  def howAreYou(temperature: Int): String =
    // (temperature > 95) ?  (isLoggedIn ? "A bit toasty." : "oops") : "Fine."
    if (temperature > 10_000) "I am dead"
    else if (temperature > 95) "A bit toasty."
    else "Fine."

  println(howAreYou(100))

}

trait Organism {
  def genus: String
  def species: String

  def binomialNomenclature = s"$genus $species"
}

class Dog(val name: String) extends Organism {
  val genus: String   = "Canis"
  val species: String = "familiaris"
}

object DogHelp extends App {
  def identify(organism: Organism): Unit =
    println(s"This looks to me like a ${organism.genus} ${organism.species}!")

  // Dog is a subtype of Organism
  // Dog <: Organism
  val sparky: Dog = throw new Error("DOG NOT IMPLEMENTED")

  // Nothing <: every other type
  // Int
  def divide(x: Int, y: Int): Int =
    if (y == 0) throw new IllegalArgumentException("Can't divide by zero") // Nothing <: Int
    else x / y                                                             // Int

  identify(sparky)     // This looks to me like a Canis familiaris!
  println(sparky.name) // COMPILER ERROR
  println(sparky.name) // COMPILER ERROR
  println(sparky.name) // COMPILER ERROR
}

object ObjectRoot {

  // Lambda & Function & Function Value
  // vs
  // Method
  def perform(f: Int => Int) = ???

  // Methods are defined on objects
  def addMethod(x: Int, y: Int): Int =
    x + y

  val add: (Int, Int) => Int =
    (x: Int, y: Int) => x + y

  addMethod(1, 2) // 3

  add(1, 2) // 3
}

object Adders extends App {

  def add(x: Int, y: Int): Int = x + y

  val numbers = List(1, 2, 3, 4, 5)

  def isEven(int: Int): Boolean = int % 2 == 0

  def isDivisibleBy(x: Int, y: Int): Boolean = x % y == 0

  println(numbers.filter(isDivisibleBy(_, 2)))

}

object LengthIsEven extends App {
  val length: String => Int  = _.length
  val isEven: Int => Boolean = _ % 2 == 0

//  val lengthIsEven: String => Boolean =
//    length andThen isEven
//    str => isEven(length(str))

  def lengthIsEven(string: String): Boolean =
    isEven(length(string))

  println(lengthIsEven("Hello")) // false
  println(lengthIsEven("xx"))    // true
}

object LookupTable extends App {
  // command - ,
  // command-shift-a

  // live templates
  def add(x: Int, y: Int): (() => Unit, Int) = {
    val logMessage: () => Unit =
      () => println(s"Adding $x and $y")
    (logMessage, x + y)
  }

  val tuple: (String, Int)             = ("Hello", 1)
  val threeple: (String, Int, Boolean) = ("Hello", 1, true)

  val fun: (String, Int) => Int =
    (str: String, int: Int) => str.length + 3
  // Writer Monads
  // Thunks
  // () => Unit
  def addFourNumbers(x: Int, y: Int, z: Int, zz: Int): (() => Unit, Int) = {
    val (log1, result)  = add(x, y)
    val (log2, result2) = add(result, z)
    val (log3, result3) = add(result2, zz)
    val totalLog: () => Unit = () => {
      log1()
      log2()
      log3()
    }
    (totalLog, result3)
  }

  val (log, result) = addFourNumbers(1, 2, 3, 4)
  println(log)

  def not(bool: Boolean): Boolean = {
    val map = Map(true -> false, false -> true)
    map(bool)
  }

  println(not(true))
  println(not(false))
}
