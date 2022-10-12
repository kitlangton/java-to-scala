package course.cohort4

object Day4 extends App {

  val double = (x: Int) => x * 2
// double(10) == 20

  val intToChar = (x: Int) => x.toChar
// intToChar(65) == 'A'

  val stringLength = (x: String) => x.length
// stringLength("HELLO") == 5

  val curriedAdd = (x: Int) => (y: Int) => x + y
// curriedAdd(10)(20) == 30

  val add10 = curriedAdd(10)
// add10(20) == 30

  val replicateCurried = (x: String) => (y: Int) => x * y
// replicateCurried("ha")(3) == "hahaha"

  val replicateUncurried = (x: String, y: Int) => x * y
// replicateUncurried("ha", 3) == "hahaha"

}

object FunkyNumbers extends App {
  import course.misc.FunWithNumbers._

  print("5 ＋ 20 = ")
  println(5 ＋ 20)

  print("10 ＋ 40 = ")
  println(10 ＋ 40)

  print("5 ＋ 10 = ")
  println(5 ＋ 10)

  println(5)
}

object FunctionLiteral extends App {
  // INSTRUCTIONS
  // ============
  // Create a function/lambda which squares its argument.

//  def squareDef(i: Int): Int = i * i
//  val square: Int => Int     = squareDef

  //    _ * _
  //    (a0, a1) => a0 * a1
  //    _ * _ + _
  //    (a0, a1, a2) => a0 * a1 + a2

//  val square: Int => Int = i => i * i
//  val add: (Int, Int) => Int = _ + _

//  val square: Int => Int = x => x * x
//  val square: Int => Int =    x => x * x

  // Int => Int
  // label of a container
  val square: Int => Int =
    (x: Int) => x * x

  val add5: Int => Int =
    (x: Int) => x + 5

  val forgetAndReturn1000: Int => Int =
    (x: Int) => 1000

  // val square: (x: Int) => Int = x * x

  val result = square(10)
  assert(result == 100, s"Expected 100, got $result")
}

object TypingLambdas extends App {
  // INSTRUCTIONS
  // ============
  // Add type signatures to the following function values
  val double: Int => Int =
    (x: Int) => x * 2
  // double(10) == 20

  val intToChar: Int => Char =
    (x: Int) => x.toChar
  // intToChar(65) == 'A'

  val stringLength: String => Int =
    (x: String) => x.length
  // stringLength("HELLO") == 5

  val curriedAdd: Int => (Int => Int) =
    (x: Int) => (y: Int) => x + y
  // curriedAdd(10)(20) == 30

// (y: Int) => 10 + y
  val add10: Int => Int =
    curriedAdd(10)
  // add10(20) == 3

  val replicateCurried: String => Int => String =
    (string: String) => (y: Int) => string * y
  // replicateCurried("ha")(3) == "hahaha"

  val replicateUncurried: (String, Int) => String =
    (string: String, y: Int) => string * y
  // replicateUncurried("ha", 3) == "hahaha"
}

object References extends App {

  class ReferencePerson(name: String, age: Int)

  case class ValuePerson(name: String, age: Int)

  val referenceMalick1 = new ReferencePerson("Reference Malick", 55)
  val referenceMalick2 = new ReferencePerson("Reference Malick", 55)

  val valueKilmer1 = ValuePerson("Value Kilmer", 38)
  val valueKilmer2 = ValuePerson("Value Kilmer", 38)

  println(referenceMalick1.toString)
  println(valueKilmer1 == valueKilmer2) // true
//  println("---")
//  println(valueKilmer1)            // ValuePerson(Value Kilmer, 38)
//  println(valueKilmer2)            // ValuePerson(Value Kilmer, 38)
//  println(valueKilmer1.hashCode()) // -1182596071
//  println(valueKilmer2.hashCode()) // -1182596071
}

object Generics1 extends App {
  // How do we deal with duplication at the type level?
  case class Pair[A](first: A, second: A)

  trait Animal
  trait Cat extends Animal
  trait Dog extends Animal

  // LUB - least upper bound
  // closest common ancestor
  // option-enter
  val other = Pair(new Cat {}, new Cat {})

  val ints: Pair[Int]                      = Pair(1, 2)
  val strings: Pair[String]                = Pair("hello", "world")
  val pairsOfBooleans: Pair[Pair[Boolean]] = Pair(Pair(true, false), Pair(false, true))

}

object Generics2 extends App {
  case class Pair[A](first: A, second: A) {
    def combinePair(combine: (A, A) => A): A =
      combine(first, second)

    def render(toString: A => String): String =
      s"A pair of ${toString(first)} and ${toString(second)}"

    def toList: List[A] = List(first, second)
  }

  def add(x: Int, y: Int): Int             = x + y
  def concat(x: String, y: String): String = x + y
  def and(b: Boolean, c: Boolean): Boolean = b && c

  def combineInts(pair: Pair[Int]): Int             = add(pair.first, pair.second)
  def combineStrings(pair: Pair[String]): String    = concat(pair.first, pair.second)
  def combineBooleans(pair: Pair[Boolean]): Boolean = and(pair.first, pair.second)

  def combinePair0[A](pair: Pair[A], combine: (A, A) => A): A =
    combine(pair.first, pair.second)

//  def and(b1: Boolean, b2: Boolean): Boolean        = b1 && b2

  // A = Int
  // A = Any
  // Any
  // multiple parameter

  def giveMeAnother[A](a: A)(another: A): A  = another
  def giveMeAnother2[A](a: A, another: A): A = another

  val cool0: Int = giveMeAnother(100)(10)
  val cool: Any  = giveMeAnother2(100, "hello")

//  def combinePair0[A](pair: Pair[A], combine: (A, A) => A): A =
//    combine(pair.first, pair.second)

  def combinePair[A](pair: Pair[A])(combine: (A, A) => A): A =
    combine(pair.first, pair.second)

  // single param list
//  combinePair0(Pair(1, 2), (x: Int, y: Int) => x + y)
//  combinePair0(Pair("hello", "world"), (x: String, y: String) => x + y)
  Pair(true, false).combinePair(_ && _)
  Pair(10, 50).combinePair(_ + _)

  // multiple param list
  combinePair(Pair(1, 2))(_ + _)

  val square: Int => Int                = x => x * x
  val squareExpanded: (Int, Int) => Int = (x: Int, y: Int) => x * y
  combinePair(Pair(1, 2))(_ + _)
}

// lambdas and generics and case classes
// to model a composable predicate DSL
// predicate: A => Boolean
// "satisfy" a predicate
// that input A returned true
// base cases. foundational/simple solutions
// combinators -> a function or method that combines/modifies one of these DSL values

// - parser DSL (String => User)
// - http DSL

// A => Boolean
// creating functions from A => Boolean from scratch
// isEven
// isOdd
// isEqualTo(10)
// modifying functions from A => Boolean
// predicate.negate
case class Digit(value: Int) {
  // combinators/methods
  // transform or combine this value
  def double: Digit = Digit(value * 2)
  def one: Digit    = Digit(1)
}

object Digit {
  // constructors
  def fromInt(int: Int) = Digit(int)
  val zero: Digit       = Digit(0)
  val one: Digit        = Digit(1)
  val fortyTwo: Digit   = Digit(42)
}

object NumberExamples extends App {
  println(Digit.fortyTwo)
}

object PredicateStuff extends App {
  // abstraction
  //
  // constructors - base cases / foundational solutions
  // combinators

  // - what do I need
  // - what do I have
  // - can I construct any piece

  // Goal: Case Class
  // Tactic: Construct Case Class
  // Effect: Case Class -> The Parameters
  // Concretely: Predicate -> (A => Boolean)

  // Goal: Lambda/Function Value
  // Tactic: Introduce a Lambda
  // Effect: (In => Out) -> Out
  // Concretely: (A => Boolean) -> Boolean (we now have an A)

  // Syntax / Semantics
  // predicate.negate -> ???
  // isEven(2) -> true
  // isEven(2).negate -> false
  // isEven(3) -> false
  // isEven(3).negate -> true

  final case class Generator[A](make: () => A) {
    def map[B](f: A => B): Generator[B] =
      Generator(() => f(make()))
  }

  // X => A           contramap
  //      A => B
  //           B => C map

  // TODO: Make predicate a trait
  // Consumer of As
  // contravariant in that type
  //     A <: B
  //  P[B] <: P[A]
  case class Predicate[A](lambda: A => Boolean) extends (A => Boolean) {

    override def apply(input: A): Boolean =
      lambda(input)

    def contramap[B](f: B => A): Predicate[B] =
      Predicate[B] { b =>
        lambda(f(b))
      }

    def negate: Predicate[A] =
      Predicate(a => !lambda(a))

    // THIS && THAT
    // isEven && isOdd =
    // isLessThanTen && isGreaterThan5 = 6,7,8,9
    // GOAL: Predicate[A]
    // 1. Construct Case Class
    // GOAL: A => Boolean
    // 2. Construct Lambda
    // GOAL: Boolean
    def and(that: Predicate[A]): Predicate[A] =
      this && that

    // symbolic alias for `and`
    def &&(that: Predicate[A]): Predicate[A] =
      Predicate[A] { a =>
        this(a) && that(a)
      }

    def ||(that: Predicate[A]): Predicate[A] =
      Predicate[A] { a =>
        this(a) || that(a)
      }

//    def ||(that: Predicate[A]): Predicate[A] =
//      (this.negate and that.negate).negate

    // a || b = !(!a and !b)

  }

  object Predicate {

    val isEven: Predicate[Int] =
      Predicate(_ % 2 == 0)

    val isOdd: Predicate[Int] =
      isEven.negate

    def lessThan(int: Int): Predicate[Int] =
      Predicate(_ < int)

    def greaterThan(int: Int): Predicate[Int] =
      Predicate(_ > int)

    def equalTo[A](a: A): Predicate[A] =
      Predicate(_ == a)
  }

//  val odds = nums.filter(Predicate.negate(Predicate.isEven).lambda)
  // a value that can determine whether or not an int is even

  val betweenThreeAndEightAndEven: Predicate[Int] =
    Predicate.greaterThan(3) &&
      Predicate.lessThan(8) &&
      Predicate.isEven

  val isGoodAccountAge: Predicate[Int] =
    betweenThreeAndEightAndEven || Predicate.equalTo(33)

  val isVerifiedUserName: Predicate[String] =
    Predicate.equalTo("John") || Predicate.equalTo("Jill")

  def isOlderThan100(int: Int): Boolean = int > 100

  final case class User(name: String, accountAge: Int)
  val dracula = User("Dracula", 1000)

  val isEven: Int => Boolean = (int: Int) => int % 2 == 0

  val userIsEvenAge: User => Boolean =
    (user: User) => isEven(user.accountAge)

  val isValidUser: Predicate[User] =
    isGoodAccountAge.contramap[User](_.accountAge) ||
      isVerifiedUserName.contramap[User](_.name)

  val users =
    List(
      User("John", 10),
      User("Jane", 20),
      User("Jack", 30),
      User("Jill", 40),
      User("Jen", 33),
      User("Yung Dracula", 6)
    )
  val result = users.filter(isValidUser)
  println(users)
  println(result)

}

object Questions extends App {

  final case class Dog(name: String)
//  class Cat(override val name: String) extends Dog(name)
//  case class Malamute(override val name: String, size: Int) extends Dog(name)
//  val georgy: Dog = new Malamute("Georgy", 100)

  def add(x: Int, y: Int): Int =
    x + y

  val curriedAdd: Int => Int => Int =
    x => y => x + y

  // (y => 10 + y)
  val add10: Int => Int =
    curriedAdd(10)

  println(add10)
  println(add10(20))

}
