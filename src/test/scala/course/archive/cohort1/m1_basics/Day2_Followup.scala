package course.archive.cohort1.m1_basics

import scala.io.StdIn.readLine

object ReliableValues {
  val myFavoriteFood: String = "pizza"
}

class GuessTheNumber(
    generateNumber: () => Int,
    handleSuccess: Int => Int,
    handleFailure: Int => Int
) {
  var score = 0

  def play(): Unit =
    while (true) {
      val result = generateNumber()
      val guess  = readLine(scala.Console.GREEN + "Your guess: " + scala.Console.RESET).toInt
      if (guess == result) {
        score = handleSuccess(score)
        println(s"CORRECT! Your score is now $score")
      } else {
        score = handleFailure(score)
        println(s"WRONG! The correct answer was $result. Your score is now $score")
      }
    }
}

object GuessTheNumber {
  val easy =
    new GuessTheNumber(
      () => scala.util.Random.nextInt(2),
      score => score + 5,
      score => score - 1
    )

  val hard =
    new GuessTheNumber(
      () => scala.util.Random.nextInt(10),
      score => score + 5,
      score => score - 10
    )

  val fixed =
    new GuessTheNumber(
      () => 0,
      score => score + 999,
      score => score + 999
    )

  def main(args: Array[String]): Unit =
    easy.play()
}

object FunctionRecap extends App {
  val addTen: Int => Int =
    (x: Int) => x + 10

  def modify[A](value: A)(f: A => A): A =
    f(value)

  val result =
    modify("HELLO")(x => x.toUpperCase)

  println(result)
}

// https://www.calculushowto.com/wp-content/uploads/2019/12/total-function.png
// The importance of precisely representing the possibilities
object Totality extends App {
  // Total / Partial
  // A total function is defined for all possible inputs
  // A partial function is defined for some possible inputs

  def renderBooleanTotal(b: Boolean): String =
    if (b) "YES!"
    else "NO!"

  def renderBooleanPartial(b: Boolean): String =
    if (b) "YES!"
    else throw new Error("I don't know what to do with this")

  def renderBooleanOption(b: Boolean): Option[String] =
    if (b) Some("YES!")
    else None

  val string = "123"

  trait User {
    def name: String
  }

  def getById(id: Int): Option[User] = ???

  def sideEffectingThing(id: Int): Unit = {
    val optionUser: Option[User] = getById(id)
    if (optionUser.isEmpty) return
    val user = optionUser.get
    println("HELLO")
  }

  // None orElse Option(1) -> Option(1)
  // Option(1) orElse Option(2) -> Option(1)

  def sideEffectingThing2(id: Int): Unit =
//    getById(id)?.name
    getById(id).map(_.name)

  def renderUsersName(id: Int): String = {
    val optionUser: Option[User] = getById(id)
    optionUser match {
      case Some(user) => user.name
      case None       => "Unknown"
    }
  }

  def renderUsersName2(id: Int): String =
    getById(id).map(user => user.name).getOrElse("Guest")

  val noneString: Option[String] = None
  val someString: Option[String] = Some("Hello")

  val optionLength: Option[Int] = someString.map(_.length)

//  val nullString: String   = null
  val actualString: String = noneString.getOrElse("Hello")

  println(someString.get)
}

object HonestyInTypes {
  import ReliableValues._

  def main(args: Array[String]): Unit =
    println(Option("HELLO").map(_.toUpperCase))

}

// THE RECOMMENDED WAY OF CREATING DATA
class PersonClass(name: String, var age: Int) {
  def birthday(): Unit =
    age += 1

  def immutableBirthday: PersonClass =
    new PersonClass(name, age + 1)
}

// ALL OF YOUR DATA SHOULD BE CASE CLASSES (and SEALED TRAITS)
//
// - apply
// - CREATE COPIES -> THE NEXT ITERATION
//final case class Pet(name: String)
//
//case class Person(name: String, age: Int, address: String) {
//  def birthday: Person =
//    copy(age = age + 1)
//}

//object Person {
//  // Static Data
//  val john = Person("John", 25)
//  val kit  = Person("Kit", 25)
//
//  // Constructors
//  def makeOldPerson(name: String) =
//    Person(name, 100)
//}

//object CaseClasses extends App {
//  val pc1 = new PersonClass("John", 30)
//  val pc2 = new PersonClass("John", 30)
//
//  // Classes have referential equality
//  println("CLASSES")
//  println(pc1 == pc2)
//  println(pc1.hashCode())
//  println(pc2.hashCode())
//  pc1.birthday()
//  println(pc1)
//  println(pc1.age)
//
//  // Value Equality
//  // Free Apply Method
//  // IMMUTABLE
//  val pcc1 = Person("John", 30)
//  val pcc2 = Person("John", 30)
//
//  println("\nCASE CLASSES")
//  println(pcc1 == pcc2)
//  println(pcc1.hashCode())
//  println(pcc2.hashCode())
//  val olderPerson = pcc1.birthday
//  println(pcc1)
//  println(olderPerson)
//
//}

object PatternMatching {
  //
}

object Collections {
  // - List

  // - Map

  // - Set
}

object Zipping extends App {
  private val ints: Array[Int]       = Array(1, 2, 3, 4)
  private val strings: Array[String] = Array("A", "B", "C", "D")

  val arrayZip: Array[(Int, String)] =
    ints zip strings

  println(ints.toList)
  println(strings.toList)
  println(arrayZip.toList)
}

object FlatMapping extends App {
  final case class User(email: String)

  final case class AppError(message: String) extends Throwable

  final case class Person(name: String, age: Int, isAlive: Boolean)

  def getUserEither(email: String): Either[AppError, User] =
    if (email.contains("x"))
      Left(AppError("Invalid email"))
    else
      Right(User(email))

  val string: String =
    getUserEither("kit.langton@gmail.com") match {
      case Left(error) => error.message
      case Right(user) => user.email
    }

  def getUser(email: String): Option[User] =
    if (email.contains("x")) None
    else Some(User(email))

  def userAliases(user: User): Option[List[String]] =
    if (user.email.contains("z")) None
    else Some(List("kit", "stacy", "bob", "joe"))

  def validateAliases(aliases: List[String]): Option[String] =
    if (aliases.length > 5) None
    else Some("THESE ALIASES ARE GREAT")

  // Array[A]
  // Array[Int]
  // Array[String]
  val arrayOfArrays: Array[Array[Int]] =
    Array(
      Array(1, 2, 3),
      Array(4, 5, 6)
    )

  val arrayOfInts: Array[Int] =
    arrayOfArrays.flatten

  // Container[Container[A]]
  // .flatten
  // Container[A]

  val arrayOfStrings: Array[String] =
    arrayOfInts.map { int =>
      s"MY INT IS $int"
    }
  // Array(1, 2, 3) -> Array("MY INT IS 1", "MY INT IS 2", "MY INT IS 3")

  // Container[A]
  // .map
  // A => B
  // Container[B]

  // Container[A]
  // .flatMap
  // A => Container[B]
  // Container[B]

  // Future <- Async
  // Future[A]
  // Task[A] <-
  // Either

  val result: Option[Int] =
    getUser("kit.langton@wiverge.com")
      .flatMap { user =>
        userAliases(user)
          .flatMap { aliases =>
            validateAliases(aliases)
              .map(_.toUpperCase.length)
          }
      }

  val result2: Option[Int] =
    for {
      user      <- getUser("kit.langton@tiverge.com")
      aliases   <- userAliases(user)
      validated <- validateAliases(aliases)
    } yield validated.toUpperCase.length

//  val whatever = {
//    val user      = getUser("kit.langton@ziverge.com")
//    val aliases   = userAliases(user)
//    val validated = validateAliases(aliases)
//    validated
//  }

  println(result2)
}
