package course.cohort4

object Day3 extends App {
  // What's the least upper bound / LUB
  // closest common ancestor

  trait Animal {
    def speak: String
  }

  trait Cat extends Animal {
    override def speak: String = "Meow"
  }

  trait Dog extends Animal {
    override def speak: String = "Woof"
  }

  final case class User(name: String)

  def ????? : Nothing = throw new Error("OOPS")

  final case class UserRepo() {
    def getUsers: List[User] = ?????
  }

  val userRepo: UserRepo  = ?????
  val results: List[User] = userRepo.getUsers

  //  val huh: Animal =
//    if (true)
//      new Dog {}
//    else
//      new Cat {}

}

object Lambdas1 extends App {
  // THE MEGA SHORTCUT - option-enter

  // Methods are defined on objects
  def addMethod(x: Int, y: Int): Int = {
    println("adding")
    x + y
  }

  // Lambdas are function values
  val add: (Int, Int) => Int = (x: Int, y: Int) => {
    println("adding")
    x + y
  }

  val addTuple: ((Int, Int)) => Int =
    (tuple: (Int, Int)) => {
      println("adding")
      tuple._1 + tuple._2
    }

  // how to call functions of functions
  // curried
  // A => B => C => D
  // (A, B, C) => D
  // => "right-associative"
  val replicate: String => Int => String =
    (string: String) => (int: Int) => string * int

  def replicateM(string: String, int: Int): String =
    string * int

  val nums: List[Int] = List(1, 2, 3, 4)

  val numsCurry: List[String] =
    nums.map(replicate("lo"))

  val strings: List[String] =
    nums.map(replicateM("lo", _))

  val resulttttt = strings.map(replicateM(_, 10))
  println(resulttttt)

  //  (3 - 2) - 1
//  3 - (2 - 1)

  val weirdFunction: (String => Int) => String =
    (stringToInt: String => Int) => stringToInt("hello").toString + "!!!"

  println("HUH!?")
//  println(weirdFunction(string => 100))

  // 1 2 3 4
  // f: Int => Int (int: Int) => int + 10
  // f: Int => String
  // "one" "two" "three" "four"

  // Int => OtherType

  // replicate: String => (Int => String)
  //
//  nums.map(Int => String)
  val mapper: Int => String = replicate("hello")
  nums.map(mapper)

  val nums2: List[String] =
    nums.map(replicate("lo"))

  println(nums)
  println(nums2)

  // un-curried
  val replicate2: (String, Int) => String =
    (string: String, int: Int) => string * int

  // stepwise evaluation
  replicate("lo")
  // 1. replace identifier with body
  ((string: String) => (int: Int) => string * int)("lo")
  // 2. function application. f(arg)
  ((int: Int) => "lo" * int)(50)
  // 3. function application. f(arg)
  "lo" * 50
  // 4.
  "lolololololo..."

//  println(replicate("lo")(50))
}

object Lambdas2 extends App {
  def add(x: Int, y: Int): Int = x + y

  val addLambda: (Int, Int) => Int  = add _
  val addLambda2: (Int, Int) => Int = add // TODO: FIX SLIDE!!!

  val addTen: Int => Int =
    x => add(10, x)

  val addTen2: Int => Int =
    x => add(x, 10)

//  val addLambda0                    = add _
//  val addLambda                     = add _
//  val addLambda1: (Int, Int) => Int = add

  def combineInts(x: Int, y: Int, combine: (Int, Int) => Int) =
    combine(x, y)

  combineInts(10, 20, add)

}

object Lambda3 extends App {
//  val length: String => Int =
//    str => {
//      println(s"length of $str")
//      str.length
//    }
//
//  val isEven: Int => Boolean =
//    int => {
//      println(s"is $int even?")
//      int % 2 == 0
//    }
  def length(str: String): Int = {
    println(s"length of $str")
    str.length
  }

  def isEven(int: Int): Boolean = {
    println(s"is $int even?")
    int % 2 == 0
  }

  val lengthIsEven: String => Boolean =
    (length _) andThen isEven

//  println(lengthIsEven("hello"))
  println(lengthIsEven("hi"))
}

object SideEffects extends App {
  // TODO: Fix Type Signature in slides

  // "truth table"
  val lookupTable: Map[Boolean, Boolean] =
    Map(
      true  -> false,
      false -> true
    )

  def not(bool: Boolean): Boolean =
    lookupTable(bool)

  val tupleSyntax: (String, Int)  = "hello" -> 10
  val tupleSyntax2: (String, Int) = ("hello", 10)

  val lookupTableAnd: Map[(Boolean, Boolean), Boolean] =
    Map(
      (true, true)   -> true,
      (true, false)  -> false,
      (false, true)  -> false,
      (false, false) -> false
    )

  def and(b1: Boolean, b2: Boolean): Boolean =
    lookupTableAnd((b1, b2))

  println(not(true))
  println(not(false))

  def uppercase(string: String): String = string.toUpperCase
  println(uppercase("hello"))
  println(uppercase("hello"))
  println(uppercase("hello"))
  println(uppercase("hello"))
}

// - types
// - subtypes (finished up)
// - lambdas
//   - function types
//   - higher order functions
// - purity
// - case classes
object PartiallyAppliedFunction extends App {

  def stringLengthMethod(string: String): Int =
    string.length

  val name: String =
    "Kit"

  val addLambda1: (Int, Int) => Int =
    (x: Int, y: Int) => x + y

  val addCurried: (Int, Int) => Int =
    (x: Int, y: Int) => x + y
  // String => Int
  // In => Out
  // val f1 = str => str.length
  // val f2 = str => str.count(_ == "a")
  // val f3 = str => 1000
  val stringLength: String => Int = // TYPE ASCRIPTION
    (string: String) => string.length // FUNCTION LITERAL

  val stringLength0 = (string: String) => string.length
  "hello"

  def add(x: Int, y: Int): Int = x + y

  val addLambda0                   = add _
  val addLambda: (Int, Int) => Int = add

  def higherOrderFunction(f: (Int, Int) => Int) = ???
  higherOrderFunction(add)

  def singleParams(x: Int, y: Int, string: String): Boolean =
    x + y > string.length

  def multiParams(x: Int, y: Int)(string: String): Boolean =
    x + y > string.length

  val strings = List("hello", "hi", "hey", "hola")

  val bools: List[Boolean] =
//    strings.map(string => multiParams(10, 20)(string))
    strings.map(multiParams(10, 20))

  val bools2: List[Boolean] =
    strings.map(singleParams(10, 20, _))
}

object Lambda4 extends App {
  val add: (Int, Int) => Int =
    (x, y) => x + y

  add(10, 20) // 30

  val add2: Int => Int => Int => Int =
    x => (y => (z => x + y))

  val result: Int => Int =
    add2(10)(20)

  println(result)

  (str: String) => {
    //
    (anotherString: String) => str + anotherString
  }

  def add3(x: Int): Int => Int =
    (y: Int) => x + y

//  def add3(x: Int)= {
//    def finish(y: Int) = x + y
//  }

  println(add3(10)(20))

}

object Example extends App {

  val length0 =
    (string: String) => string.length

  val length: String => Int =
    string => string.length

  val isEven: Int => Boolean =
    int => int % 2 == 0

  val lengthIsEven =
    (string: String) => isEven(length(string))

  // method versions of the above

  def lengthMethod(string: String): Int =
    string.length

  // _ = turn this method name into a function
  val myFunction: String => Int =
    lengthMethod

  def isEvenMethod(int: Int): Boolean =
    int % 2 == 0

  def lengthIsEvenMethod(string: String): Boolean =
    isEvenMethod(lengthMethod(string))

  lengthIsEvenMethod("hello")
}
