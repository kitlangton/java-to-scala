package course.cohort4

// Scala
// - HotSpot JIT
// - expressiveness: high-level, precision, concision
// Systems
// - Rust AOC
// - DevOps
//
// - side projects
//   - toy version
//   - minimize contrivance
//     - tool
// - toy problems
//   - no leetcode
//   - adventofcode
// - open source
//   - reading real open
//   - motivation
// - teaching
// - Red Book
final case class Day7()

object CustomBoolean extends App {
  import Boolean._

  sealed trait Boolean

  object Boolean {
    case object True  extends Boolean
    case object False extends Boolean
  }

  val ex2: String =
    if (true) "hello" else "cool"

  val trueValue: Boolean  = True
  val falseValue: Boolean = False

  // lazy
  // call-by-() parameter
  // call-by-name parameter
  def greet(name: => String): Unit = {
    name
    name
    name
    println(s"Hello how are you?")
  }

//  greet(getName)

  def getName: String = {
    println("I am getting my name...")
    "Kit"
  }

  // eager evaluation
  // lazy evaluation
  // thunk : () => A
  //         A
  // A => B
  def ifThenElse[A](cond: Boolean, ifTrue: => A, ifFalse: => A): A = {
    println(s"MATCHING $cond")
    cond match {
      case Boolean.True  => ifTrue
      case Boolean.False => ifFalse
    }

  }

  ifThenElse(True, println("IT'S TRUE!"), println("NO IT IS NOT!"))

//  val ex3 = ifThenElse(False, 1, 0)
//  val ex4 = ifThenElse(False, "good", "bad")
}

object Exampleeee extends App {
  def eagerEvaluation(x: Long): Unit = {
    println("by value: " + x)
    println("by value: " + x)
  }

  // lazy evaluation
  def calledByName(x: => Long): Unit = {
    println("by name: " + x)
    println("by name: " + x)
  }

  // Here the value is evaluated eagerly.
  eagerEvaluation(System.nanoTime())

  // Here the value will be evaluated only when it is called.
  // (or) Lazily evaluated
  calledByName(System.nanoTime())
}

trait NumberAgain { self =>
  def value: Int

  def increment: NumberAgain =
    new NumberAgain {
      override def value: Int = self.value + 1
    }
}

object SerDes extends App {
  final case class User(name: String, age: Int)
  // save to disk
  // { "name": "sanoteu", "age": 12 }

  // Codec | Encoder Decoder
  // Serdes | Serializer Deserializer

  // Serialization
  // User => String
  // User => Array[Byte]

  // User => SavableSendableFormat

  // Deserialization
  // String => User // partial
  // Array[Byte] => User // partial
}

object NumberAgainExample extends App {
  val one = new NumberAgain {
    override def value: Int = 1
  }
  println(one.value)
  println(one.increment.value)
}

object UserExample extends App {
  // case classes - Product AND Person(name, age)
  // sealed trait - Sum     OR True OR False
  sealed trait User { self =>
    def greet: String =
      self match {
        case User.Registered(name) => s"Hello, $name!"
        case User.Guest            => "Hello, Guest!"
      }

    def introduce(that: User): String =
      (self, that) match {
        case (User.Registered(name1), User.Registered(name2)) =>
          s"$name1, meet $name2!"

        case (User.Registered(name1), User.Guest) =>
          s"$name1, quickly, this way, a filthy guest is approaching!"

        case (User.Guest, User.Registered(name1)) =>
          s"$name1, quickly, this way, a filthy guest is approaching!"

        case (User.Guest, User.Guest) =>
          s"Quickly, guests, fight amongst yourselves!"
      }
  }

  object User {
    case class Registered(name: String) extends User
    case object Guest                   extends User

    def greet(user: User): String =
      user match {
        case User.Registered(name) =>
          s"Welcome back, $name! How can I help you?"
        case User.Guest =>
          "Hmph."
      }
  }

//  println(User.greet(User.Registered("Kit")))
//  println(User.greet(User.Guest))
//  println(User.Registered("Kit").greet)
//  println(User.Guest.greet)
  println(User.Guest introduce User.Registered("Kit"))
}

//class LinkedInts(value: Int, var next: LinkedInts = null) {
//}

object Parent {
  val scopeValue = "FUN"

  trait Human { self =>
    def name: String

    def lastName: String = "Nonclone"

    def introduce: Unit = println(s"hello my name is ${this.name}")

    def clone(newName: String): Human =
      new Human {
        override def name: String = s"$newName (clone of ${self.name})"

        override def lastName: String = "Clonerson"
      }

  }

  object HumanUse extends App {
    val kit = new Human {
      override def name: String = "Kit"
    }
    kit.introduce

    val jason = new Human {
      override def name: String = "jason"
    }
    jason.introduce
  }
}

object DataRecursion extends App {

  // List, Map, Set, Vector
  sealed trait IntList { self =>
    def ::(int: Int): IntList =
      Prepend(int, self)

    def sum: Int =
      self match {
        case Prepend(number, tail) => number + tail.sum
        case Empty                 => 0
      }
  }

  def sum(ints: IntList): Int =
    ints match {
      case Prepend(number, tail) => number + sum(tail)
      case Empty                 => 0
    }

  case class Prepend(number: Int, tail: IntList) extends IntList
  case object Empty                              extends IntList

  val ints123: IntList   = 1 :: 2 :: 3 :: Empty
  val ints12: IntList    = 1 :: 2 :: Empty
  val ints1: IntList     = 2 :: 1 :: Empty
  val intsEmpty: IntList = Empty

  println(ints123)
  println(sum(ints123))

  //

  final case class User(name: String, friend: User)
  final case class UserL(name: String, friend: () => UserL)

  object UserL {
    def make(name: String, friend: => UserL): UserL =
      new UserL(name, () => friend)
  }

  lazy val kit: UserL = UserL.make("Kit", kit)
  println(kit)
//  println(kit.friend)
//  println(kit.friend.friend)
}

object ExampleStringList extends App {
  sealed trait List[+A]

  case class Prepend[A](head: A, next: List[A]) extends List[A]
  case object Nil                               extends List[Nothing]

  val emptyString: List[Nothing] = Nil
  val strings                    = Prepend("Hello", Prepend("World", Prepend("!!!", emptyString)))

  def concat(strings: List[String]): String =
    strings match {
      case Prepend(head, next) => head ++ concat(next)
      case Nil                 => ""
    }

  concat(strings) // "HelloWorld!!!"
}

object CrazyThing extends App {

  final case class <>(name: String, age: Int)
  val crazy: <> = <>("kit", 32)
  crazy match {
    case name <> age =>
      println(s"Name: $name, Age: $age")
  }
}

object GenericList extends App {

  // variance covariant +

  // List[A]
  // - Cons(A, List[A])
  // - Nil

//  enum List[A]:
//    case Cons(head: A, tail: List[A])
//    case Nil

  sealed trait List[+A] extends Product with Serializable

  object List {
    // lisp cons, ::, prepend
    final case class Cons[A](head: A, tail: List[A]) extends List[A]
    case object Nil                                  extends List[Nothing]
  }

}

object CaseObjectSyntax extends App {
  // nullary case class
  final case class Blob()
  val blob1 = Blob() // new object on heap
  val blob2 = Blob() // new object on heap
  println(blob1)
  println(blob2)
  println(blob1 == blob2)

  case object Plob // singleton
  val plob1 = Plob // same object on heap
  val plob2 = Plob // same object on heap
  println(plob1)
  println(plob2)
  println(plob1 == plob2)
}

// agda - executable math proof
// dependent types
object CollectionStuff extends App {

  val solmization: List[String] =
    List("do", "re", "mi", "fa", "so", "la", "ti")

  val other: Vector[String] =
    Vector("do", "re", "mi", "fa", "so", "la", "ti")

  val stringLength: String => Int =
    (string: String) => string.length

  val string: String = "hello".reverse
  val cool           = string.reverse

  def takeSeq(strings: Seq[String]) = ???

  takeSeq(solmization)
  takeSeq(other)
  // var args
  List(1, 2, 3)

}

sealed trait Color extends Product with Serializable {
  // we want to implement a fruit method apple for RED and blueberry for Blue
//  def fruit: String
  def fruit: String = this match {
    case Color.Red  => "apple"
    case Color.Blue => "blueberry"
  }
}

object Color extends App {

  def getFruit(color: Color): String = color match {
    case Red  => "Apple"
    case Blue => "Blueberry"
  }

  case object Red extends Color {
//    override def fruit: String = "apple"
  }

  case object Blue extends Color {
//    override def fruit: String = "blueberry"
  }

  val red: Color  = Red
  val blue: Color = Blue
  println(red.fruit)
  println(blue.fruit)
}
