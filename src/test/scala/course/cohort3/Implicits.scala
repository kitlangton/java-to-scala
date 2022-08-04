package course.cohort3

object Implicits {
  // What are they?
  // 1. Implicit parameters
  // 2. Implicit conversions `A => B`
  //
  // Patterns
  // - convenience (implicit ec: ExecutionContext)
  //  - (implicit config: Config)
  // - type classes (Monoid, Semigroup, Functor, Monad, etc) (JsonCodec, Avro, etc)
  // - extension methods
  // - dependently typed functions

  val string = "hello" //.spongebobCase
}

object ImplicitParameters extends App {
  // extra parameter lists, whose parameters will be passed to the function at compile

  def addFive(x: Int): Int = x + 5
//  println(addFive(5))

  //  implicit val stupidInt: Int = 20

  // implicit resolution:
  // √
  // for any implicit Type there can only be one valid candidate

//  def addFiveToImplicitInt(implicit x: Int): Int = x + 5
//
//  implicit val magicString: String                    = "Hello"
//  implicit def magicInt(implicit string: String): Int = string.length
//
//  // 1. local definitions
//  println(addFiveToImplicitInt)
}

object LocalResolution {
  def gimmeInt(implicit x: Int): Unit = println(x)

  implicit val localInt: Int = 10

  gimmeInt
}

object AnotherObject {
  implicit val inAnotherObject: Int = 10
}

object ExplicitImportResolution {
  import AnotherObject.inAnotherObject

  def gimmeInt(implicit x: Int): Unit = println(x)

  gimmeInt

  val myName: String = "Kit"
}

object WildcardImportResolution {
  import AnotherObject._

  def gimmeInt(implicit x: Int): Unit = println(x)

  gimmeInt
}

trait ImplicitDefs {
  implicit val upthechain: Int = 10
}

trait IntermediaryTrait extends ImplicitDefs

object UpTheInheritanceChainResolution extends IntermediaryTrait {
  def gimmeInt(implicit x: Int): Unit = println(x)

  gimmeInt
}

final case class Thing(name: String) {}
object Thing {
  implicit val thing: Thing         = Thing("Something")
  implicit val thingBox: Box[Thing] = Box(Thing("Something"))
}

object TheCompanionObjectResolution extends App {
  def implicitly[A](implicit a: A): A = a
  println(implicitly[Thing])
}

final case class Box[A](value: A)
object Box {
  implicit val intBox: Box[Int]       = Box(10)
  implicit val stringBox: Box[String] = Box("Hello")
}

object TheInceptionCompanionObjectResolution extends App {
  // search the companion objects of the type arguments
  println(implicitly[Box[Thing]])
}

final case class SocialSecurityNumber(value: Int) extends AnyVal

object LookingAtStuff extends App {

  // bundles of methods that relate to some type a
  trait Show[A] {
    def show(a: A): String
  }

  object Show {
    def show[A](a: A)(implicit showA: Show[A]): String = showA.show(a)

    implicit val showString: Show[String] =
      new Show[String] {
        def show(a: String): String = s"\"$a\""
      }

    implicit val showInt: Show[Int] =
      new Show[Int] {
        def show(a: Int): String = a.toString
      }

    implicit val showSocialSecurityNumber: Show[SocialSecurityNumber] =
      new Show[SocialSecurityNumber] {
        def show(a: SocialSecurityNumber): String = s"<REDACTED>"
      }

    implicit def showList[A](implicit showA: Show[A]): Show[List[A]] = new Show[List[A]] {
      def show(as: List[A]): String = as.map(showA.show).mkString(", ")
    }
  }

  val ss          = SocialSecurityNumber(123456789)
  val manySocials = List(SocialSecurityNumber(123456789), SocialSecurityNumber(987654321))
  println(Show.show(manySocials))

  // Typeclass Pattern
  // trait or case class that's parameterize by some type A
  // and then a series of canonical implicit definitions for that trait/case class
//  final case class JsonEncoder[A](encode: A => String)
//  object JsonEncoder {
//    implicit val stringEncoder: JsonEncoder[String] =
//      JsonEncoder(str => s"\"$str\"")
//  }
//
//  final case class Pet(name: String)
//  object Pet {
//    implicit val petEncoder: JsonEncoder[Pet] =
//      JsonEncoder(pet => s"""{"name": "${pet.name}"}""")
//  }
//
//  final case class Sandwich(numberOfPickles: Int)
//
//  object DeriveJsonEncoder {
//    def gen[A]: JsonEncoder[A] = ???
//  }
//
//  object Sandwich {
//    implicit val sandwichEncoder: JsonEncoder[Sandwich] =
//      JsonEncoder(sandwich => s"""{"pickles": ${sandwich.numberOfPickles}}""")
//  }
//
//  def sendToClient[A](as: List[A])(implicit encoder: JsonEncoder[A]): Unit =
//    as.foreach(a => println(encoder.encode(a)))
//
//  sendToClient(List("Hello", "Goodbye", "Why"))
//  sendToClient(List(Pet("Kit"), Pet("Spongebob")))
//  sendToClient(List(Sandwich(10), Sandwich(20)))
//
//  def appLogic[A](getItems: () => List[A])(implicit encoder: JsonEncoder[A]): Unit = {
//    println("Starting app logic")
//    val items = getItems()
//    println(s"Got items $items")
//    sendToClient(items)
//  }

}
