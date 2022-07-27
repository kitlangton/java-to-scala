package course.cohort3

import pprint.pprintln

object Day3 extends App {
  println("Hello, Day 3!")
}

object FunctionLiteral2 extends App {
  // INSTRUCTIONS
  // ============
  // Create a function/lambda which squares its argument.
  //       (Int, String): String                 FINAL RESULT
  def indentMethod(spaces: Int, string: String): String = ???
  val indent: Int => String => String = { spaces => s =>
    (" " * spaces) + s
  }

  //  val indent: Int => String => String =
//    spaces => str => (" " * spaces) + str

//  pprintln(10 * " ")
}

object IndentFunction extends App {
  //  _ + 5
  //  x => x + 5
  // _ => _ + 5

  def indent(spaces: Int, string: String): String =
    (" " * spaces) + string

  val strings = List("a", "b", "c")
  pprintln(strings.map(indent(5, _)))
}

object AddOne {
  def apply(x: Int): Int = x + 1
}

object EdsgerSharing extends App {

  println(AddOne(2))

  // class with special methods and limitations
  // Swift struct
  case class Person(name: String, age: Int, list: List[Int])

  // apply constructor
  val edsger: Person = Person("Edsger Wybe Dijkstra", 32, List(1, 2, 3))
  val oldsger        = edsger.copy(name = "Oldsger") // YES YES YES!
  // value vs reference
  // mutable.HashMap
  // immutable.Map

//  pprintln(edsger)  // Person(Edsger Wybe Dijkstra, 32)
//  pprintln(oldsger) // Person(Edsger Wybe Dijkstra, 100)
//  pprintln(edsger)  // Person(Edsger Wybe Dijkstra, 32)
//  pprintln(oldsger) // Person(Edsger Wybe Dijkstra, 100)
}

//object Math101 extends App {
//  import course.lessons.misc.FunWithNumbers._
//  import zio.Console
//
//  // mutable numbers!
//  // mutable numbers is sad!
//  println(5 ＋ 5)  // 10
//  println(10 ＋ 5) // 15
//  println(5 ＋ 10) // ???
//  println(5)
//  println(10)
//}

object TypeParams extends App {
  // How do we deal with duplication at the type level?
  case class Pair[A](first: A, second: A)

  val ints  = Pair(1, 2)
  val other = Pair("string", "String")

  def combineInts(pair: Pair[Int]): Int             = pair.first + pair.second
  def combineStrings(pair: Pair[String]): String    = pair.first ++ pair.second
  def combineBooleans(pair: Pair[Boolean]): Boolean = pair.first && pair.second

  def combineBooleans2(pair: Pair[Boolean]): Boolean = pair.first || pair.second
  def combineBooleans3(pair: Pair[Boolean]): Boolean = !pair.first || pair.second
  def combineBooleans4(pair: Pair[Boolean]): Boolean = true
  def combineBooleans5(pair: Pair[Boolean]): Boolean = false

  def combinePair[A](pair: Pair[A], combine: (A, A) => A): A =
    combine(pair.first, pair.second)

  def identity[A](a: A): A = a
}

final case class Email(string: String) {
  def getDomain: String = ???
}

// signal intention
// root methods
// case class
// wrapping predicates in case classes
final case class Predicate[A](run: A => Boolean) {
  def or(that: Predicate[A]): Predicate[A] =
    Predicate(input => run(input) || that.run(input))

  def and(that: Predicate[A]): Predicate[A] =
    Predicate(input => run(input) && that.run(input))

  def not: Predicate[A] =
    Predicate(a => !run(a))
}

object Predicate {
  def equalTo[A](value: A): Predicate[A] =
    Predicate[A](input => input == value)

  Predicate.equalTo(10)
  // Predicate[Int](input => input == 10)
  // (input: Int) => input == 10
  // _ == 10
  Predicate.equalTo("howdy")
  // Predicate[String](input => input == "howdy")
  // (input: String) => input == "howdy"
  // _ == "howdy
  Predicate.equalTo(10) or Predicate.equalTo(15)
  // (n) => n == 10              (n) => n == 15
  // input => { (n => n == 10)(input) || (n => n == 15)(input) }
  // input => { input == 10 || (n => n == 15)(input) }
  // input => input == 10 || input == 15
  //

  def contains[A](list: List[A]): Predicate[A] =
    Predicate(a => list.contains(a))

  // varargs
  def any[A](predicates: Predicate[A]*): Predicate[A] =
    Predicate((a: A) => predicates.foldLeft(false)((acc, predicate) => acc || predicate.run(a)))
}

object Predicate2 {
  // CONSTRUCTORS
  def equalTo[A](value: A): A => Boolean =
    (input: A) => input == value

  def greaterThan(int: Int): Int => Boolean =
    (input: Int) => input > int

  def lessThan(int: Int): Int => Boolean =
    (input: Int) => input < int

  def greaterThanOrEqualTo(int: Int): Int => Boolean =
    or(greaterThan(int), equalTo(int))

  def lessThanOrEqualTo(int: Int): Int => Boolean =
    or(lessThan(int), equalTo(int))

  // COMBINATORS

  def or[A](p1: A => Boolean, p2: A => Boolean): A => Boolean =
    (input: A) => p1(input) || p2(input)

  def and[A](p1: A => Boolean, p2: A => Boolean): A => Boolean =
    (input: A) => p1(input) && p2(input)
}

object PredicateTests extends App {

  final case class Event(audience: Int, isImportant: Boolean)

  val isImportant    = (event: Event) => event.isImportant
  val hasBigAudience = (event: Event) => event.audience > 10

  val eventPredicate = Predicate2.and(isImportant, hasBigAudience)

  println("IS BIG EVENT?")
  println(eventPredicate(Event(5, isImportant = true)))
  println("--")

  val eqTen: Int => Boolean =
    Predicate2.equalTo(10)
  val ltTen: Int => Boolean =
    Predicate2.lessThan(10)

  val composed: Predicate[Int] =
    Predicate.equalTo(5) or
      Predicate.equalTo(7) or
      Predicate.equalTo(10)

  val contains: Predicate[Int] =
    Predicate.contains(List(100, 15))

  val lessThanOrEqualToTen =
    Predicate2.and(
      Predicate2.or(eqTen, ltTen),
      Predicate2.greaterThan(3)
    )

  val composedAny =
    Predicate.any(composed, contains, Predicate.equalTo(37)).not
  println(composedAny.run(7))

//  println(eqTen(10)) // true
//  println(eqTen(5))  // false
//  println(ltTen(10)) // false
//  println(ltTen(5))  // True
//  println(lessThanOrEqualToTen(11)) // false
//  println(lessThanOrEqualToTen(10)) // true
//  println(lessThanOrEqualToTen(5))  // true
//  println(lessThanOrEqualToTen(2))  // false

}
