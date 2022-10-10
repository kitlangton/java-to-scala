package course.cohort4

import scala.language.implicitConversions

object Day6 {
  // implicits
  // the compiler transforming your code at compile time
  // 1. implicit parameters
  //    - inserting additional parameters into a function
  //    - someFunc(1,2)
  //    - f1: someFunc(1,2)(add, more, params)
  //    - f2: someFunc(1,2)(???)
  //
  // Implicit Resolution
  // the algorithm/rules by which the compiler finds OR constructs values
  // to use as implicit parameters
  //
  // 2. implicit conversions
  //    - converting one type to another
  //    - a
  //    - conversion(a)
  //
}

object ImplicitParameterExploration extends App {

  def addOneExplicit(int: Int): Int = int + 1

  println(addOneExplicit(10))
//  addOneExplicit

  // Common/Possible Patterns
  // 0. convenience. you don't have to manually pass around implicit values
  // 1. type classes
  // 2. dependently typed functions
  // 3. ergonomics
  // computationThatCouldFail.orElseSucceed(User.default)
  // computationThatAlwaysSucceeds //.orElseSucceed(User.default)

  def addTen(int: Int) = int
  // definition site
//  def add(x: Int)(y: Int): Int                  = x + y
  def addImplicit(x: Int)(implicit y: Int): Int = x + y

  // Command-B or command-click
  // Cmd-[ Cmd-]
  implicit val trueValue: Boolean = true

  implicit def helloWorld(implicit boolean: Boolean): String =
    if (boolean) "hello world"
    else "goodbye world"

  implicit def stringLengthI(implicit
      string: String,
      boolean: Boolean
  ): Int = if (boolean) string.length else string.length * 2
  //  implicit val oneHundred: Int = 100
//  implicit val ten: Int        = 10
  // How Implicit Resolution works = ???

  // 1. if you mark a param list as implicit
  // AND 2. you call that method without providing those arguments
  // the compiler will perform a SEARCH for values of those types
  // implicit search begins at the call site
//  println(addOneI(stringLengthI(helloWorld)))
  println(addImplicit(100))
}

final case class Thing(name: String)

trait Implicits {
  val thing: Thing = Thing("Foobar")
}

trait IntermediateTrait extends Implicits {
  val cool = 120
//  implicit val jonny: Imp = Imp("Jonny")
}

// Implicit resolution
// 1. All of your intuitions for bringing values/methods into scope, apply to implicits
// 2. Companion Object of the outermost type
//    a. The Companion Objects of any type arguments
// Resolution <: IntermediateTrait <: Implicits
object Resolution1 extends App with IntermediateTrait {
  println(cool)
  println(thing)
  println(implicitly[Imp])
}

final case class Imp(name: String)

object Imp {
  // IR will search the companion object of the type
  implicit val beelzebub: Imp = Imp("Beelzebub")
  //  implicit val jonny: Imp     = Imp("Jonny")
}

// Library Code
final case class Box[A](value: A)

object Box {}

// Our Code
final case class HotDog(deliciousness: Int)

object HotDog {
  implicit val boxHotDog2: Box[HotDog] = Box(HotDog(10_000))
}

object GenericResolution extends App {
  implicitly[Box[HotDog]]
}

object ImplicitConversions extends App {
  // 1. You are using some value of type A, where
  //    the compiler infers that you need a type B
  //    implicit conversion A => B

  // PathLiteral("users") / In.int / PathLiteral("posts") / In.int
  // "users" / In.int / "posts" / In.int
  // "users/10/posts/20"
  final case class Name(string: String)

  implicit val int: Int = 10

  // implicit def can have no non-implicit arguments
  implicit def helloWorld(implicit boolean: Boolean): String =
    if (boolean) "hello world"
    else "goodbye world"

  // implicit conversion will have ONE NON IMPLICIT argument
  implicit def string2Name(string: String): Name = Name(string)

  implicit def int2Name(int: Int): Name = Name(int.toString)

  val kit: Name = "Kit"

  def greetName(name: Name): Unit = println(s"Hello, ${name.string}")

  greetName("Jimmy")

  // 1. You are using some value of type A, where
  //    the compiler infers that you need a type B
  //    implicit conversion A => B
  // 2. A.method BUT that doesn't exist on A, BUT
  //    does exist on some type B
  //    implicit conversion A => B

  //  final case class StringOps(string: String) {
  //    def yell: String = string.toUpperCase + "!!!"
  //  }
  //
  //  implicit def string2StringOps(string: String): StringOps =
  //    StringOps(string)

  implicit class StringOps(self: String) {
    def yell: String = self.toUpperCase + "!!!"

    def spongebobCase: String =
      self
        .grouped(2)
        .map {
          case s if s.length == 2 => s"${s(0).toUpper}${s(1).toLower}"
          case s                  => s"${s(0).toUpper}"
        }
        .mkString("")
  }

  println("how are you doing".spongebobCase)

  final case class DancingComputer(power: Int) {}

  implicit def computer2DancingComputer(computer: Computer): DancingComputer =
    DancingComputer(computer.power)

  final case class Computer(power: Int) {
    def dance: Unit =
      println(s"WOW IM DANCING! IM REALLY DANCING!")
  }

  val computer = Computer(1)
  computer.dance

}

object PatternMatching extends App {
  def identify(any: Any): String =
    any match {
      case int: Int       => s"An Int $int"
      case string: String => s"A String ${string.toUpperCase}"
      case _: Double      => "A Double"
      case _: Boolean     => "A Boolean"
      case _              => "Unknown"
    }

//  println(identify(10))
//  println(identify("hello"))

  case class Person(name: String, age: Int, isAlive: Boolean)

  def greet(person: Person): String =
    person match {
      case Person(name, age, true) =>
        s"Hello, $name, who is $age years old!"
      case Person(name, age, false) =>
        s"AHHHH! OH NO! A ZOMBIE named $name who was $age."
    }

  val kit: Person       = Person("Kit", 32, true)
  val zombieKit: Person = Person("Kit", 32, false)
//  println(greet(kit))
  println(greet(zombieKit))
}

object PetPhone extends App {

  case class PhoneNumber(areaCode: Int, exchange: Int, line: Int)

  case class Person(name: String, pet: Pet)

  case class Pet(name: String, phoneString: String)

  val phoneNumberRegex = "(\\d{3})-(\\d{3})-(\\d{4})".r

  // ideas, reifying, encoding their behavior as values
  // partial function
  // PartialFunction

  def personsPetsPhoneNumber(person: Person): PhoneNumber =
    person match {
      case Person(_, Pet(_, phoneNumberRegex("555", exchange, line))) if line.toInt > 7000 =>
        PhoneNumber(555, exchange.toInt, line.toInt)
      case _ =>
//        PhoneNumber(100, 100, 100)
        throw new IllegalArgumentException("Invalid pet number")
    }

  val biff = Person("Biff", Pet("Biff Jr.", "555-456-5890"))
  println(personsPetsPhoneNumber(biff).areaCode + 10) // PhoneNumber(555, 456, 7890)
}

// - PartialFunction
// - Variance/Covariance/Contravariance
// - Type Classes
//   - Context Bounds
object Example000 extends App {
  trait Typeclass[A] {
    def foo: Int
  }

  object Typeclass {

    def apply[A](implicit typeclass: Typeclass[A]): Typeclass[A] =
      typeclass

//    def foo[A](implicit typeclass: Typeclass[A]): Int =
//      typeclass.foo

    implicit val int: Typeclass[Int] = new Typeclass[Int] {
      def foo = 1000
    }

    implicit val string: Typeclass[String] = new Typeclass[String] {
      def foo = 500
    }
  }

  // Type
  // Boolean {true, false}
  // true    {true}
  // false         {false}
  // true  <:< Boolean
  // false <:< Boolean
  // Int     {0,1,2}
  // 1       {1}
  // 1 | 2   {1, 2}
  // (1, 2)
  // Nothing {}
  // Nothing <:< 1 <:< Int

  // Type Classes
  // specific examples
  // "context bounds" syntactic sugar

  // precision vs. ergonomics/pragmatism

  // 1 | 2
  // tagged union
  // Int except 0
  // 2^32 - 1 + *
  // def divide(int: Int, y: Int except 0)

  val oneAndTwo: (1, 2) = (1, 2)
//  val oneOrTwo: 1 | 2 = 1
  // singleton types
  val TRUE: true = true

  def method(param: Boolean): Boolean = param
  method(TRUE)

//  def getTypeclass[A](value: A)(implicit typeclass$A$0: Typeclass[A]) =
//    println(typeclass$A$0.foo)

  def getTypeclass[A: Typeclass](value: A): Unit =
    println(Typeclass[A].foo)

//  getTypeclass(10)
}

// trait, class, etc.
final case class Coffee(blend: String, caffeine: Int) {}

object Coffee {
  def makeCoffee(blend: String): Coffee =
    Coffee(blend, 100)
}

object Example100 extends App {
  println(
    Coffee.makeCoffee("Moroccan")
  )
}
