package course.cohort4

import scala.annotation.tailrec
import scala.reflect.ClassTag

// Remaining Topics
// - Tail Recursion
// - Scala Data Structures | Sealed Traits (Option, Either, List)
// - Functional Data Modeling
// - Type Classes
// - Future
// - Effect Systems
object Day8 extends App {
  val ints: List[Int] = List(1, 2, 3, 4, 5)
  println(ints.appended(10))
  println(ints.prepended(10))
  val prepended = ::(10, ints)
  // disjoint union
  // (A, B)
  // A | B
  // tagged union
  // val x: Int | String = "hello"
  val x0: Either[Int, String] = Right("hello")
  val x1: Either[Int, String] = Left(12)
  val y: (Int, String)        = (12, "hello")
}

object TailRecursion extends App {
  // The functional equivalent of a while loop
  // while loop -> tail recursive
  // while loop <- tail recursive

  // recursion
  def F(n: Int): Int =
    n match {
      case 0          => 0
      case 1          => 1
      case _ if n > 1 => F(n - 1) + F(n - 2)
    }

  // - no while/for looping!
  def fibTailRec(n: Int): Int = {
    @tailrec
    def loop(i: Int, curr: Int, next: Int): Int =
      if (i < n)
        loop(i + 1, curr = next, next = curr + next)
      else
        curr

    loop(i = 0, curr = 0, next = 1)
  }

  def fibWhile(n: Int): Int = {
    var i    = 0
    var curr = 0
    var next = 1

    while (i < n) {
      val temp = curr
      curr = next
      next = temp + next
      i += 1
    }

    curr
  }

//  F(40)
//  F(39) + F(38)
//  ((F(37) + F(36)) + F(37)) + (F(37) + F(36))

  // intToLetter: Int => Char

  // List[A] => List[B]
  // 1 2 3 4 5
  // V V V V V
  // a b c d e

  // List[Config] => List[AwsCredentials]
  // f:       Config => AwsCredentials
  // configs.map(configToCredentials)

  // implement map with a while loop
  // FP: map, foreach, filter
  def map[A, B](list: List[A])(f: A => B): List[B] = {
    var curr: List[A] = list
    var acc: List[B]  = Nil

    while (curr != Nil) {
      acc = f(curr.head) :: acc
      curr = curr.tail
    }

    acc.reverse
  }

  println((0 to 10).map(F))
  println((0 to 1000).map(fibWhile))
  println((0 to 1000).map(fibTailRec))
}

object ListMethods extends App {
  val solmization: List[String] = List("do", "re", "mi", "fa", "so", "la", "ti")

  val sins: List[String] = List("gluttony", "lust", "pride", "sloth", "wrath", "envy", "greed")

  val zipped: List[(String, String)]         = solmization zip sins
  val mapped: Map[String, String]            = zipped.toMap
  val unzipped: (List[String], List[String]) = zipped.unzip
  println(zipped)
  println(mapped)

}

object OptionStuff extends App {
//  sealed trait Option[+A]
//
//  final case class Some[A](value: A) extends Option[A]
//  case object None                   extends Option[Nothing]

  // Some[A] <: Option[A]
  // None    <: Option[A]
  val some10: Option[Int]  = Some(10)
  val noneInt: Option[Int] = None

  val userInput: String    = ???
  val userInt: Option[Int] = userInput.toIntOption

  // true -> "true"
  // true 1283 aoeu -> "true 1283 aeou"
  val input: String = scala.io.StdIn.readLine()

  // String => Option[A]
//  def processInputNull(input: String): Boolean =
//    input match {
//      case "yes" => true
//      case "no"  => false
//      case _     => null
//    }

  trait Animal
  trait Dog extends Animal
  trait Cat extends Animal

  def processInput(input: String): Option[Boolean] =
    if (input == "yes")
      Some(true)
    else if (input == "no")
      Some(false)
    else
      None
  //    input match {
//      case "yes" => Some(true)
//      case "no"  => Some(false)
//      case _     => None
//    }

//  def processInput(input: String): Animal =
//    input match {
//      case "yes" => new Dog
//      case "no"  => new Dog
//      case _     => new Cat
//    }

  def processInput[A](input: A): Option[Any] =
    Some(input)

  val example = ???
  // "1000"  -> Some(1000)
  // "hello" -> None

  // Example Implementation
  def getOrElse[A](option: Option[A])(fallback: => A): A =
    option match {
      case Some(value) => value
      case None        => fallback
    }

  val favoriteChild: Option[String] = None

  val food = getOrElse(Some("Pizza")) {
    println("DEBUG!")
    "Sushi"
  }
  println(food)

  val child = favoriteChild.getOrElse {
    println("I HAVE NO CHILD")
    "Sushi Steve"
  }
  println(child)

}

object EitherStuff extends App {
  final case class User(name: String)
  final case class UserProfile(name: String, info: String)

  def getUser(id: Int): Either[String, User] =
    if (id == 1) Right(User("Biff"))
    else if (id == 2) Right(User("Shishu"))
    else Left("User not found")

  def getUserProfile(user: User): Either[String, UserProfile] =
    if (user.name == "Biff") Right(UserProfile("Biff", "Cool Guy"))
    else Left(s"User Profile not found for ${user.name}")

  // Either[A]
  // f: A => B
  // B => Either[String, UserProfile]
  // Either[B]
  // Either[Either[String, UserProfile]
  val result1: Either[String, String] =
    getUser(1)
      .flatMap { user =>
        getUserProfile(user)
          .map(_.info)
      }

//  val result2 =  {
//    val user    = getUser(1)
//    val profile = getUserProfile(user)
//    profile.info
//  }

  val a     = 0 + 1     // 1
  val three = a + a + a // 2
  println(a + three) //3

  flatMap(0 + 1) { a =>
    flatMap(a + a + a) { three =>
      println(a + three)
    }
  }

  def flatMap[A, B](expr: A)(f: A => B): B =
    f(expr)

  // Monad -> sequential/imperative
  def flatMap[E, A, B](either: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
    either match {
      case Left(e)  => Left(e)
      case Right(a) => f(a)
    }

  getUser(1).flatMap { user =>
    val specialValue = 100
    getUserProfile(user).map { profile =>
      profile.info + specialValue
    }
  }

  // a list with zero or one elements
  val option: Option[Int] = Option(10)

  val result2: Either[String, Unit] =
    for {
      user <- getUser(2)
      _    <- getUserProfile(user)
    } yield ()

  println(result1)
  println(result2)
}

object Exp100 extends App {
  sealed trait List[+A]                            extends Product with Serializable
  final case class Cons[A](head: A, tail: List[A]) extends List[A]
  case object Nil                                  extends List[Nothing]

  sealed trait Option[+A]           extends Product with Serializable
  final case class Some[A](head: A) extends Option[A]
  case object None                  extends Option[Nothing]
}

object Variance extends App {
  // relates to subtyping
  // how subtyping behaves w/r/t type parameters
  trait Animal {
    def name: String
  }
  trait Dog extends Animal {
    def name         = "Dog"
    def woof: String = "woof"
  }
  trait Cat extends Animal {
    def name         = "Cat"
    def meow: String = "hello"
  }

  val animal: Animal = new Animal {
    def name: String = "Animal"
  }

  val dog: Dog = new Dog {}

  // Consumer
  //        () => A
  //               Produce
  class MutableBox[+A](value: A) {

    // consume
//    def setValue(a: A): Unit = ???
//    def getValue: A = ???
  }
  //       Animal
  //      /     \
  // [Dog]        Cat
  val mutDog: MutableBox[Dog] = new MutableBox[Dog](dog)
//  val mutAnimal: MutableBox[Animal] = mutDog //.asInstanceOf[MutableBox[Animal]]
//  mutAnimal.value = new Cat {}
//  println(mutDog.value.woof)

  // invariant
  val dogArray: Array[Dog] = Array(dog, dog)
  // Array[Dog] <: Array[Animal]
  val animalArray: Array[Animal] = dogArray.asInstanceOf[Array[Animal]]
  animalArray(0) = new Cat {}
//  dogArray.foreach(_.woof)

  //     -> widen
  // Dog <: Animal
  //     <- narrow YOU CANT
  val dogAsAnimal0: Animal = dog

  // + covariance, that the box is Covariant A
  // The Container[A] either Containers or Produces some type A
  //     A  <:     B
  // Box[A] <: Box[B]
  final case class Box[+A](value: () => A)
  val animalBox: Box[Animal] = Box(() => animal)
  val dogBox: Box[Dog]       = Box(() => dog)

  //     -> widen
  // Dog      <: Animal
  val dogAsAnimal: Animal            = dog
  val dogBoxAsAnimalBox: Box[Animal] = dogBox

}

object OptionCovariance extends App {

  // invariant
  sealed trait Option[+A] extends Product with Serializable {
//    def method(a: A): Int = 10
  }

  final case class Some[A](value: A) extends Option[A]
  case object None                   extends Option[Nothing]

  val none: Option[Nothing] = None
  val someInt: Option[Int]  = Some(10)

  // Nothing <: Int
  // Option[Nothing] <! Option[Int]
  val result: Option[Int] =
    if (true) None
    else someInt
}

object OptionVsNull extends App {

  // A <: B
  // A <:< B
//  trait <:<[A, B] extends (A => B)
  trait IsSubtypeOf[Sub, Super] {
    def widen(sub: Sub): Super
  }

  // A <: A
  implicit def isSubtypeOf[A]: IsSubtypeOf[A, A] =
    new IsSubtypeOf[A, A] {
      override def widen(sub: A): A = sub
    }

  final case class Box[A](value: A) {
    // A => Int
    def double(implicit ev: A IsSubtypeOf Int): Box[Int] =
      Box(ev.widen(value) * 2)
  }

  println(implicitly[ClassTag[Box[Int]]])
  val arrStr: Array[String] = Array.apply("1", "2")

  val boxInt: Box[Int] = Box(10)
  // Int => Int
  println(boxInt.double)
  val boxString: Box[String] = Box("Hello")
  // String
//  println(boxString.double)
//  println(boxString.double)
  // Lists[A]
  // Lists[(A,B)] unzip toMap

  final case class Thing() {
    def bar: String = "hello"
  }

  // Thing OR null
  val thing0: Thing = Thing()
  val thing1: Thing = null
  thing0.bar

//  if (thing1 == null) println("IM NOT HERE")
//  else if (thing1.isInstanceOf[Thing]) println("IM A THING")

//  thing1 match {
//    case null    => println("IM NOT HERE")
//    case Thing() => println("IM A THING")
//  }

  // Some(A) OR None
  //     Animal
  // Dog       Cat

  //      Option[A]
  //  Some[A]   None
  // Some[A] <: Option[A]
  final case class Person(name: String)

  val thingO0: Some[Thing] =
    Some(Thing()) // constructors | not matchers
//  Person("Name")

  val thingO1: Option[Thing] =
    None

  thingO1 match {
    case Some(value) => println(value)
    case None        => println("none")
  }
//  println(thingO1.getOrElse(Thing()))
}

// when you create Option[A], it comes with Some or None

//

object OptionExamples extends App {
  val input: String = null

  final case class Person(name: String, age: Int)

  private val maybeString: Option[String] = Option.apply(input)
//  new Option[] {}

  // Option[A] -> A
  // None        (fallback: A) -> fallback
  // Some(value) (fallback: A) -> value
  val string: String = maybeString.getOrElse("generic username")

  // Option[String] => String
  //                  (String, Int) => Person
  val person: Person = Person(string, 32)
  // map
}

object EitherExamples extends App {
  // Either right-biased
  // E or A
  // map(A => B)
  // .left.map(E => F)

  val input: String = null
  final case class Person(name: String, age: Int)

  private val eitherString: Either[Error, String] = Right(input)
  //  new Option[] {}

  // Option[A] -> A
  // Left(error)  (fallback: A) -> fallback
  // Right(value) (fallback: A) -> value
  val name: String = eitherString.getOrElse("generic username")

  // Option[String] => String
  //                  (String, Int) => Person
  val person: Person = Person(name, 32)
}

object SimpleOption extends App {
  final case class User(name: String)
  final case class Account(number: Int)

  def stringToInt(input: String): Option[Int] = {
    println(s"stringToInt($input)")
    input.toIntOption
  }
  // Error | User
  def getUser(id: Int): Option[User] = {
    println(s"getUser($id)")
    None
  }
  def getUserAccount(user: User): Option[Account] = {
    println(s"getUserAccount($user)")
    None
  }

  // for-comprehension IS flatMaps
  val thing: Option[Account] =
    for {
      input: String    <- Some("123")
      id: Int          <- stringToInt(input)
      user             <- getUser(id)
      account: Account <- getUserAccount(user)
    } yield account

  val thing2: Option[Account] =
    Some("hello")
      .flatMap { (input: String) =>
        stringToInt(input)
          .flatMap { (id: Int) =>
            getUser(id)
              .flatMap { user =>
                getUserAccount(user)
                  .map { (account: Account) =>
                    account
                  }
              }
          }
      }

  println(s"thing = $thing")
}

object SimpleNull extends App {
  final case class User(name: String)
  final case class Account(number: Int)

  // Int != Array[Int]
  // String != User
  // String != Option[String]
  def stringToInt(input: String): Int     = input.toInt
  def getUser(id: Int): User              = ???
  def getUserAccount(user: User): Account = ???

  // worry about the happy path
  // short-circuiting
  try {
    val input: String    = "123"
    val id: Int          = stringToInt(input)
    val user: User       = getUser(id) // !!!
    val account: Account = getUserAccount(user)
    println(account)
  } catch {
    case e: Throwable => println(s"Something went wrong $e!")
  }
}
