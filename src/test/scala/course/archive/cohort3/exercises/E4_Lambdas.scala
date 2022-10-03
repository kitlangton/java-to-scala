package course.archive.cohort3.exercises

import course.archive.cohort2.exercises.{Exercise, Guess}
import course.archive.cohort2.exercises.Exercise.???
import course.archive.cohort2.exercises.Guess.guessType
import course.archive.cohort2.exercises.SomeType.same
import zio.test.TestAspect.ignore
import zio.test._

import scala.annotation.tailrec

/** Functions are fundamental to functional programming. In a functional
  * programming language, functions are values, which may be stored in fields,
  * accepted as arguments to other functions, and returned from functions.
  * So-called "first-class functions" go by many names in different programming
  * languages, including "closures", "lambdas", and "anonymous functions".
  * Effectively understanding and using functions is a pre-requisite to the
  * mastery of functional programming, in Scala or any other programming
  * language.
  *
  * In this module, you will learn about "functions as values" and become
  * comfortable creating, transforming, composing, and typing functions.
  */
object E4_Lambdas extends Exercise {

  /** ✏ EXERCISE
    *
    * Create a lambda which squares its argument and store the lambda into the
    * `square` variable
    */
  val squareTest = test("square") {
    val square: Int => Int = ???

    assertTrue(square(3) == 9)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Create a lambda with two arguments, which adds them together, and store
    * the lambda into the `plus` variable.
    */
  val plusTest = test("plus") {
    val plus: (Int, Int) => Int = ???

    assertTrue(plus(2, 2) == 4)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Use the `_` to create a lambda that adds one to its argument.
    */
  val underscoreTest = test("underscore") {
    val addTwo: Int => Int = ???

    assertTrue(addTwo(2) == 4)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Use `Function#andThen` to compose two of the following functions together
    * to create a composite function that counts the number of digits inside an
    * integer.
    */
  val andThenTest = test("andThen") {
    val convertToString: Int => String = _.toString
    val countLength: String => Int     = _.length

    val numberOfDigits: Int => Int = ???

    assertTrue(numberOfDigits(123) == 3)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Use `Function#compose` to compose two of the following functions together
    * to create a composite function that counts the number of digits inside an
    * integer.
    *
    * Which one do you prefer, `compose` or `andThen`?
    */
  val composeTest = test("compose") {
    val convertToString: Int => String = _.toString
    val countLength: String => Int     = _.length

    val numberOfDigits: Int => Int = ???

    assertTrue(numberOfDigits(123) == 3)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Use `identity` to create a function that returns its same string argument.
    */
  val identityTest = test("identity") {
    val sameString: String => String = ???

    assertTrue(sameString("foobar") == "foobar" && sameString("barfoo") == "barfoo")
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Use `Function.const` to create a function that always returns 42,
    * regardless of whichever string argument is passed.
    */
  val constTest = test("const") {
    val cool: String => Int = ???

    assertTrue(
      cool("foo") == cool("bar"),
      cool("foobar") == 42,
      cool("STRING") == 42
    )
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Create a function which returns a function. The returned function should
    * prepend the specified number of spaces to the string that it is passed.
    */
  val prependSpaceTest = test("prependSpace") {
    val prependSpace: Int => (String => String) = ???

    assertTrue(prependSpace(5)("foo") == "     foo")
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Create a function that returns a function transformer. The function
    * transformer should take a `String => String` function, and then return a
    * new `String => String` function that will repeat the original function the
    * specified number of times.
    */
  val repeatTest = test("repeat") {
    @tailrec
    def repeat0(n: Int, f: String => String)(string: String): String =
      if (n == 0) string
      else repeat0(n - 1, f)(f(string))

    lazy val repeat: Int => (String => String) => String => String =
      (n: Int) =>
        (f: String => String) =>
          (string: String) =>
            if (n == 0) string
            else repeat(n - 1)(f)(f(string))

    def addDot(string: String): String = string + "."
    assertTrue(repeat(5)(addDot)("Coming soon") == "Coming soon.....")
  }

  def exercise =
    suite("Functions as Values")(
      squareTest,
      plusTest,
      underscoreTest,
      andThenTest,
      composeTest,
      identityTest,
      constTest,
      prependSpaceTest,
      repeatTest
    )
}

object FunctionTypes {

  def assertTypeEquals[A, B](implicit ev: A <:< B) =
    assertCompletes

  /** ✏ EXERCISE
    *
    * Determine the type of `f1`, and place this (function) type in the space
    * provided. If you are correct, then the test will compile.
    */
  val f1 = (x: Int) => x * x

  val guess1 = Guess[???] // <- EDIT HERE

  guessType(f1)(guess1)

  /** ✏ EXERCISE
    *
    * Determine the type of `f2`, and place this (function) type in the space
    * provided. If you are correct, then the test will compile.
    */
  val f2 = (x: Int, y: Int) => x + y

  val guess2 = Guess[???] // <- EDIT HERE

  guessType(f2)(guess2)

  /** ✏ EXERCISE
    *
    * Determine the type of `f3`, and place this (function) type in the space
    * provided. If you are correct, then the test will compile.
    */
  val f3 = (t: (Int, Int)) => t._1 + t._2

  val guess3 = Guess[???] // <- EDIT HERE

  guessType(f3)(guess3)

  /** ✏ EXERCISE
    *
    * Determine the type of `f4`, and place this (function) type in the space
    * provided. If you are correct, then the test will compile.
    */
  val f4 = (x: Int) => (y: Int) => x + y

  val guess4 = Guess[???] // <- EDIT HERE

  guessType(f4)(guess4)

  /** ✏ EXERCISE
    *
    * Determine the type of `f5`, and place this (function) type in the space
    * provided. If you are correct, then the test will compile.
    */
  val f5 = (x: Int) => (g: Int => Int) => g(x)

  val guess5 = Guess[???] // <- EDIT HERE

  guessType(f5)(guess5)

}

/** Partial functions are functions that are defined for some inputs, but not
  * for all inputs.
  */
object PartialFunctions extends Exercise {

  /** ✏ EXERCISE
    *
    * Define a `divide` partial function that is only defined when the second
    * component of the tuple is non-zero.
    */
  val divideTest = test("divide") {
    val divide: PartialFunction[(Int, Int), Int] =
      ???

    assertTrue(
      !divide.isDefinedAt((42, 0)),
      divide.isDefinedAt((8, 4)),
      divide.unapply((8, 4)).contains(2)
    )
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Define a `toInt` PartialFunction that is only defined when the string can
    * be converted to an `Int`.
    */
  val toIntTest = test("toInt") {
    val toInt: PartialFunction[String, Int] = { case s if s.matches("-?[0-9]+") => s.toInt }

    assertTrue(
      !toInt.isDefinedAt("foo"),
      toInt.isDefinedAt("42"),
      toInt.unapply("42").contains(42)
    )
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Define a `divideOption` method by using the `PartialFunction#lift` method.
    */
  val liftTest = test("lift") {
    val divide: PartialFunction[(Int, Int), Int] = {
      case (x, y) if y != 0 => x / y
    }

    def divideOption: ((Int, Int)) => Option[Int] = ???

    assertTrue(
      divideOption((42, 0)).isEmpty,
      divideOption((8, 4)).contains(2)
    )
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Complete the PartialFunction in the body of `collect` which filters out
    * the odd numbers, and turns the even numbers into strings.
    */
  val collectTest =
    test("collect") {
      val numbers = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      val evenStrings: List[String] = numbers.collect { // Complete this PartialFunction
        case _ => ???
      }

      assertTrue(evenStrings == List("2", "4", "6", "8", "10"))
    } @@ ignore

  def exercise =
    suite("Partial Functions")(
      divideTest,
      toIntTest,
      liftTest,
      collectTest
    )
}

/** Predicates are functions that return a Boolean value. The type of a
  * predicate is `A => Boolean`.
  */
object PredicateExercise {

  type Predicate[-A] = A => Boolean

  object Predicate {

    // # CONSTRUCTORS
    // Constructors are ways to create a type.

    val always: Predicate[Any] = ???

    val never: Predicate[Any] = ???

    def equalTo[A](value: A): Predicate[A] = ???

    def lessThan(value: Int): Predicate[Int] = ???

    def greaterThan(value: Int): Predicate[Int] = ???

    def between(lower: Int, upper: Int): Predicate[Int] = ???

    def outside(lower: Int, upper: Int): Predicate[Int] = ???

    // # COMBINATORS
    // Combinators are ways to combine a type.

    def and[A](p1: Predicate[A], p2: Predicate[A]): Predicate[A] = ???

    def or[A](p1: Predicate[A], p2: Predicate[A]): Predicate[A] = ???

  }

  val intPredicate: Predicate[Int] =
    Predicate.or(Predicate.equalTo(42), Predicate.between(100, 200))

  def printPredicate[A](predicate: Predicate[A], value: A): Unit = {
    val result = predicate(value)
    println(s"$value is $result")
  }

  def main(args: Array[String]): Unit =
    printPredicate(intPredicate, 42)

}

/** Parsers are a great example of the power of lambdas. A parser can be viewed
  * as nothing more than a lambda. Functions can construct or combine parsers,
  * providing the ability to compositionally specify how to parse any type of
  * data.
  *
  * In this graduation project, you will gain experience constructing lambdas,
  * including higher-order lambdas which themselves accept and return lambdas.
  */
object LambdasGraduation {
  type Parser[+A] = String => Either[String, (String, A)]

  object Parser {

    /** ✏ EXERCISE
      *
      * Implement a parser that succeeds with the specified value, but does not
      * consume any input.
      */
    def succeed[A](a: => A): Parser[A] = ???

    /** ✏ EXERCISE
      *
      * Implement a parser that fails with the specified message, and does not
      * consume any input.
      */
    def fail(message: => String): Parser[Nothing] = ???

    /** ✏ EXERCISE
      *
      * Implement a parser that consumes any character, or fails if there are no
      * characters left to consume.
      */
    def anyChar: Parser[Char] = ???

    /** ✏ EXERCISE
      *
      * Implement a parser that parses only the specified character, or fails
      * with a message indicating which character was expected.
      */
    def char(char: Char): Parser[Unit] = ???
  }

  implicit class ParserExtensionMethods[A](self: Parser[A]) {

    def map[B](f: A => B): Parser[B] = self.flatMap(a => Parser.succeed(f(a)))

    /** ✏ EXERCISE
      *
      * Implement a function that can feed the output value of this parser into
      * the provided callback, which can return a new parser which will be fed
      * the leftover input of this parser.
      */
    def flatMap[B](f: A => Parser[B]): Parser[B] = ???

    def ~[B](that: => Parser[B]): Parser[(A, B)] =
      self.flatMap(a => that.map(b => (a, b)))

    def <~(that: => Parser[Any]): Parser[A] = (self ~ that).map(_._1)

    def ~>[B](that: => Parser[B]): Parser[B] = (self ~ that).map(_._2)

    /** ✏ EXERCISE
      *
      * Implement a function that will try to parse using the left-hand side,
      * but if that fails, it will try to parse using the right-hand side.
      */
    def |(that: => Parser[A]): Parser[A] = ???

    def repeat: Parser[List[A]] =
      (self ~ repeat).map { case (head, tail) =>
        head :: tail
      } | Parser.succeed(List.empty[A])

    def optional: Parser[Option[A]] =
      self.map(Option(_)) | Parser.succeed(Option.empty[A])

    def run(input: String): Either[String, A] = self(input).map(_._2)
  }

  def readFile(file: String): String = {
    val source = scala.io.Source.fromFile(file)

    try source.getLines().mkString("\n")
    finally source.close()
  }

  sealed trait CSVData
  object CSVData {
    final case class Header(names: List[String])   extends CSVData
    final case class Values(columns: List[String]) extends CSVData
  }

  /** ✏ EXERCISE
    *
    * Implement a function to parse the contents of a CSV file into a list of
    * CSV data elements.
    */
  def parseFile(contents: String): Either[String, List[CSVData]] = ???

  def main(args: Array[String]): Unit = {
    val contents = readFile(args(0))
    val parsed   = parseFile(contents).getOrElse(throw new RuntimeException(s"Error parsing $contents"))
    println(parsed.mkString("\n"))
  }

}
