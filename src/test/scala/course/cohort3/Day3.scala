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
final case class Predicate[-A](run: A => Boolean) {
  def or[A1 <: A](that: Predicate[A1]): Predicate[A1] =
    Predicate(input => run(input) || that.run(input))

  def and[A1 <: A](that: Predicate[A1]): Predicate[A1] =
    Predicate(input => run(input) && that.run(input))

  def not: Predicate[A] =
    Predicate(a => !run(a))
}

object Predicate {
  // Numeric
//  def greaterThan(int: Int): Predicate[Int] =
//    Predicate[Int](input => input > int)

//  def greaterThan(double: Double): Predicate[Double] =
//    Predicate[Double](input => input > double)
//
//  def greaterThan(long: Long): Predicate[Long] =
//    Predicate[Long](input => input > long)
//
//  def greaterThan(float: Float): Predicate[Float] =
//    Predicate[Float](input => input > float)

  trait Comparable[A] {
    def gt(lhs: A, rhs: A): Boolean
  }

  object Comparable {
    implicit val intComparable: Comparable[Int] = new Comparable[Int] {
      override def gt(lhs: Int, rhs: Int): Boolean = lhs > rhs
    }

    implicit val doubleComparable: Comparable[Double] = new Comparable[Double] {
      override def gt(lhs: Double, rhs: Double): Boolean = lhs > rhs
    }

    implicit val longComparable: Comparable[Long] = new Comparable[Long] {
      override def gt(lhs: Long, rhs: Long): Boolean = lhs > rhs
    }
  }

  def greaterThan[A](value: A)(implicit numeric: Numeric[A]): Predicate[A] =
    Predicate[A](input => numeric.gt(input, value))

  def lessThan[A](value: A)(implicit numeric: Numeric[A]): Predicate[A] =
    Predicate[A](input => numeric.lt(input, value))

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

object PredicateTests extends App {

  val equalTo5: Predicate[Int]          = Predicate.equalTo(5)
  val equalTo5Double: Predicate[Double] = Predicate.equalTo(5.0)
  val gt5: Predicate[Int]               = Predicate.greaterThan(5)
  val gt5Double: Predicate[Double]      = Predicate.greaterThan(5.0)
  val gt5Long: Predicate[Long]          = Predicate.greaterThan(5L)

  println(gt5.run(6))
  println(gt5.run(5))
  println(gt5Double.run(6.0))
  println(gt5Double.run(5.0))
  println(gt5Long.run(6L))
  println(gt5Long.run(5L))

//  println(eqTen(10)) // true
//  println(eqTen(5))  // false
//  println(ltTen(10)) // false
//  println(ltTen(5))  // True
//  println(lessThanOrEqualToTen(11)) // false
//  println(lessThanOrEqualToTen(10)) // true
//  println(lessThanOrEqualToTen(5))  // true
//  println(lessThanOrEqualToTen(2))  // false

}
