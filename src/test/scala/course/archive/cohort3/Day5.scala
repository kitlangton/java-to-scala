package course.archive.cohort3

import pprint.pprintln

object Day5 extends App {
  // 0. Exercises
  // 1. Use Numeric typeclass
  // 2. Implicit Conversions
  // 3. Pattern Matching
  // 4. Sealed Traits
  // - https://exercism.io
  // - https://adventofcode.com

  def identify(any: Any): String =
    any match {
      case int: Int    => s"${int + 100} is an Int"
      case str: String => s"${str.toUpperCase} is a String"
      case _: Double   => "A Double"
      case _: Boolean  => "A Boolean"
      case _           => "Unknown"
    }
//
//  pprintln(identify(1))
//  pprintln(identify("Hello"))
//  pprintln(identify(1.0))
//  pprintln(identify(true))
//  pprintln(identify(()))

  case class Person(name: String, age: Int)

  def greet(person: Person): String =
    person match {
      // Extractor
      case Person(name, age) => s"Hello, $name, who is $age years old!"
    }

  pprintln(greet(Person("John", 30)))

  def compare(person1: Person, person2: Person): String =
    (person1, person2) match {
      case (Person(name1, age1), Person(name2, age2)) if age1 > age2 =>
        s"$name1 is older than $name2"
      case (Person(name1, age1), Person(name2, age2)) if age2 > age1 =>
        s"$name2 is older than $name1"
      case (Person(name1, _), Person(name2, _)) => // if age1 == age2
        s"$name1 and $name2 are the same age"
    }

  val biff     = Person("Biff", 100)
  val theodora = Person("Theodora", 35)
  compare(theodora, biff) // "Biff is older than Theodora"
  compare(biff, biff)     // "Biff and Biff are the same age"

  case class PhoneNumber(areaCode: Int, exchange: Int, line: Int)
  val phoneNumberRegex  = "(\\d{3})-(\\d{3})-(\\d{4})".r
  val phoneNumberRegex2 = "\\((\\d{3})\\) (\\d{3})-(\\d{4})".r

  def parsePhoneNumber(phoneString: String): PhoneNumber =
    phoneString match {
      case phoneNumberRegex(areaCode, exchange, line) =>
        PhoneNumber(areaCode.toInt, exchange.toInt, line.toInt)
      case phoneNumberRegex2(areaCode, exchange, line) =>
        PhoneNumber(areaCode.toInt, exchange.toInt, line.toInt)
      case str =>
        throw new IllegalArgumentException(s"Invalid phone number: $str")
    }

  pprintln(parsePhoneNumber("(123) 456-7890")) // PhoneNumber(123, 456, 7890)
//  parsePhoneNumber("12356-7-8900") // IllegalArgumentException: Invalid phone number
//  parsePhoneNumber("BANANAPHONE!") // IllegalArgumentException: Invalid phone number

}

object NewThing extends App {
  case class PhoneNumber(areaCode: Int, exchange: Int, line: Int)
  case class Person(name: String, pet: Pet)
  case class Pet(name: String, phoneString: String)

  val phoneNumberRegex = "(\\d{3})-(\\d{3})-(\\d{4})".r

  def personsPetsPhoneNumber(person: Person): PhoneNumber =
    person match {
      case Person(_, pet @ Pet(_, phoneNumberRegex("555", exchange, line))) if line.toInt < 10_000 =>
        println(s"HERE IS MY PET: $pet")
        PhoneNumber(555, exchange.toInt, line.toInt)
      case _ =>
        throw new IllegalArgumentException("Invalid pet number")
    }

  val biff = Person("Biff", Pet("Biff Jr.", "555-456-7890"))
  println(personsPetsPhoneNumber(biff)) // PhoneNumber(555, 456, 7890)

}

object PredicateCombinator extends App {
  // INSTRUCTIONS
  // ============
  // Implement the or combinator for Predicate

  trait PredicateTrait[A] { self =>
    def run(a: A): Boolean

    def or(that: PredicateTrait[A]): PredicateTrait[A] =
      new PredicateTrait[A] {
        override def run(a: A): Boolean =
          self.run(a) || that.run(a)
      }
  }

  // A => Boolean
  case class Predicate[A](run: A => Boolean) { self =>

    // Complete this combinator
    def or(that: Predicate[A]): Predicate[A] =
      Predicate[A](a => self.run(a) || that.run(a))
  }

  object Predicate {
    def equalTo[A](expected: A): Predicate[A] =
      Predicate[A](_ == expected)
  }

  val hello: Predicate[String] =
    Predicate.equalTo("hello")

  val equalsTenOrTwenty: Predicate[Int] =
    Predicate.equalTo(10) or Predicate.equalTo(20)

  assert(equalsTenOrTwenty.run(10), s"10 should be equal to 10!")
  assert(equalsTenOrTwenty.run(20), s"20 should be equal to 20!")
  assert(!equalsTenOrTwenty.run(15), s"15 should NOT be equal to 10!")

}

object CustomBoolean extends App {
  sealed trait Boolean

  object Boolean {
    case object True  extends Boolean
    case object False extends Boolean
  }

  def ifThenElse[A](cond: Boolean, ifTrue: => A, ifFalse: => A): A =
    cond match {
      case Boolean.True  => ifTrue
      case Boolean.False => ifFalse
    }

  def printAndReturn[A](value: A): A = {
    println(s"I HAVE $value")
    value
  }

  println(
    ifThenElse(
      cond = Boolean.True,
      ifTrue = printAndReturn(100),
      ifFalse = printAndReturn(200)
    )
  )

}

object UserExhaustivityExample extends App {
  sealed trait User

  object User {
    case class Registered(name: String) extends User
    case object Guest                   extends User
  }

  def greet(user: User): String =
    user match {
      case User.Registered("Kit") => "YOU STINK!"
      case User.Registered(name)  => name
      case User.Guest             => "GUEST"
    }

  println(greet(User.Registered("John"))) // "John"
  println(greet(User.Registered("Kit")))  // "John"
  println(greet(User.Guest))              // "GUEST"

}

object IntListExamples extends App {
  // sealed case class Hello(name: String)

  // 1. case class + sealed traits
  // 2. trait - type classes, DSLs
  // 3. class - low-level mutable things

  sealed trait IntList { self =>
    def head: Int =
      self match {
        case IntList.Prepend(number, _) => number
        case IntList.Empty              => throw new Exception("EMPTY LIST HAS NO HEAD!")
      }

    def tail: IntList =
      self match {
        case IntList.Prepend(_, tail) => tail
        case IntList.Empty            => throw new Exception("EMPTY LIST HAS NO TAIL!")
      }
  }

  object IntList {
    case class Prepend(number: Int, rest: IntList) extends IntList
    case object Empty                              extends IntList
  }

  import IntList._

  val ints: IntList = Prepend(1, Prepend(2, Prepend(3, Empty)))

  pprintln(ints.tail.tail.tail)

}

object GenericList extends App {
  // - optimization
  // - user experience
  // - inference

  val strings: List[String] =
    "Hello" :: "World" :: "!!!" :: Nil

  def concat(strings: List[String]): String =
    strings match {

      case head :: next => head ++ concat(next)
      case Nil          => ""
    }

  def sum(ints: List[Int]): Int =
    ints match {
      case head :: next => head + sum(next)
      case Nil          => 0
    }

  val numbers = List(1, 2, 3, 4)
  pprintln(numbers.sum)
//  pprintln(concat(strings))               // "HelloWorld!!!"
//  pprintln(sum(::(1, ::(2, ::(3, Nil))))) // 6

}

object Zipping extends App {
  val solmization: List[String] = List("do", "re", "mi", "fa", "so", "la", "ti")

  val sins: List[String] = List("gluttony", "lust", "pride", "sloth", "wrath", "envy", "greed")

  val zipped: List[(String, String)] = solmization zip sins

  val unzipped: (List[String], List[String]) = zipped.unzip

//  pprintln(solmization)
//  pprintln(sins)
//  pprintln(zipped)
//  pprintln(List(1, 2, 3).sum)

  val numbers     = List(1, 2, 3, 4, 5, 6)
  val listOfLists = numbers.grouped(4).toList
}
