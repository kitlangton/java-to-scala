package course.cohort4

import course.cohort4.syntax._

object TypeClasses {
  // THE PATTERN
  trait Typeclass[A] {
    def specialValue: A
    def operation(a: A, b: A): A
  }

  object Typeclass {
    def apply[A](implicit tc: Typeclass[A]): Typeclass[A] = tc

    implicit val intTypeclass: Typeclass[Int] = new Typeclass[Int] {
      override def specialValue: Int              = 0
      override def operation(a: Int, b: Int): Int = a + b
    }
  }

  object TypeclassSyntax {
    implicit class TypeclassOps[A](a: A) {
      def operation(b: A)(implicit tc: Typeclass[A]): A = tc.operation(a, b)
    }
  }

  // code pattern — it comes from Haskell (typeclass)
  // implicits (parameters-conversions)
  // traits
  //
  // typeclass - a bundle/pattern of behavior
  // - some operations
  // - some values
  // - some laws
  // Monoid
}

final case class HumanBeing(
    name: String,
    age: Int,
    ssns: List[SSN]
)

object HumanBeing {
//  implicit val jsonEncoder: JsonEncoder[A] = DeriveJsonEncoder.gen[HumanBeing]
  // {
  //   "name": JsonEncoder.encode(user.name),
  //   "age": JsonEncoder.encode(user.age),
  //   "ssns": JsonEncoder.encode(user.ssns)
  // }

}

sealed trait Version extends Product with Serializable
object Version {
  case object One extends Version
  case object Two extends Version
}

// ADTs = case classes and sealed traits
final case class Region(name: String)

class Config0 {
  def typeString: String = ???
  def region: Region     = ???
  def version: Version   = ???
  def port: Int          = ???
}
sealed trait Config extends Product with Serializable

object Config {
  final case class AwsConfig(region: Region, version: Version) extends Config
  final case class LocalConfig(port: Int)                      extends Config
}
//case class Config(version: Version, port: Int, region: Region)
//object Config {
//  val test: Config = Config(version = Version.Two, port = 8080)
//  val prod: Config = Config(version = Version.One, port = 80)
//}

//trait Config {
//  def version: Version
//  def port: Int
//}
//
//trait TestConfig extends Config {
//  def version: Version = Version.One
//  def port: Int        = 8080
//}
//trait ProdConfig extends Config {
//  def version: Version = Version.Two
//  def port: Int        = 80
//}

//trait MyConfig   extends Config
//trait YourConfig extends Config
//trait OurConfig  extends MyConfig with YourConfig

// 1. one canonical instance per typeclass
trait SchemaEncoder[A] {
  def encode(a: A): String
  // string
}

trait SchemaEncoderV2[A] {
  def encode(a: A, version: Version): String
}

// circe/zio.json
trait JsonDecoder[A] {
  def decode(json: String): Option[A]
}

trait JsonEncoder[A] {
  def encode(a: A): String
}

//object Json {
//  def toJson[A: JsonEncoder](a: A): String =
//    implicitly[JsonEncoder[A]].encode(a)
//
//  def fromJson[A: JsonDecoder](json: String): Option[A] =
//    implicitly[JsonDecoder[A]].decode(json)
//}

trait Show[A] {
  def show(a: A): String
}

object Show {

  // bespoke of implicitly
  def apply[A](implicit show: Show[A]): Show[A] = show

  def show[A: Show](a: A): String = Show[A].show(a)

  // "instance" of the Show typeclass for Int
  implicit val showInt: Show[Int] = new Show[Int] {
    def show(a: Int): String = a.toString
  }

  implicit val showString: Show[String] = new Show[String] {
    def show(a: String): String = a
  }

  implicit val showBoolean: Show[Boolean] = new Show[Boolean] {
    def show(a: Boolean): String = a.toString
  }

  implicit val showDouble: Show[Double] = new Show[Double] {
    def show(a: Double): String = a.toString
  }

  implicit def showList[A: Show]: Show[List[A]] = new Show[List[A]] {
    def show(as: List[A]): String =
      as.map(_.show).mkString("[", ", ", "]")
  }

  // Map[K, V]
  implicit def showMap[K: Show, V: Show]: Show[Map[K, V]] = new Show[Map[K, V]] {
    def show(map: Map[K, V]): String =
      map.map { case (k, v) => s"${k.show} -> ${v.show}" }.mkString("{", ", ", "}")
  }
}

final case class SSN(int: Int)

object SSN {
  implicit val showSSN: Show[SSN] = new Show[SSN] {
    def show(a: SSN): String = "SSN<REDACTED>"
  }
}

object AnotherObject extends App {
  val ints   = List(1, 2, 3)
  val string = List("a", "b", "c")
  val bools  = List(true, false, true)
  val ssns   = List(SSN(123), SSN(456), SSN(789))

  // def showList2[A: Show](list: List[A]): String =
  def showList[A](list: List[A])(implicit show: Show[A]): String =
    list.map(show.show).mkString(" - ")

  // context bound - syntactic sugar
  def showList2[A: Show](list: List[A]): String =
    list.map(Show[A].show).mkString(" - ")

  val mySSN = SSN(123)

  val mapOfListsOfSSNS: Map[String, List[SSN]] =
    Map(
      "a" -> List(SSN(123), SSN(456)),
      "b" -> List(SSN(789), SSN(101))
    )

  println(ints.show)
  println(ssns.show)
  println(mapOfListsOfSSNS.show)
}

object syntax {
  // A => ShowOps
  implicit class ShowOps[A](a: A) {
    def show(implicit show: Show[A]): String = show.show(a)
  }
}

// trait with a single type parameter
trait Monoid[A] {
  def empty: A
  def combine(lhs: A, rhs: A): A
  // laws
  // 1. a • empty == a             right identity
  // 2. empty • a == a             left identity
  // 3. (a • b) • c == a • (b • c) associativity
}

object Monoid extends App {
  implicit val intAddition: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(lhs: Int, rhs: Int): Int =
      lhs + rhs
  }

  val intMultiplication =
    new Monoid[Int] {
      def empty: Int = 1
      def combine(lhs: Int, rhs: Int): Int =
        lhs * rhs
    }

//  final case class Max(int: Int)
  val maxInt =
    new Monoid[Int] {
      def empty: Int = Int.MinValue
      def combine(lhs: Int, rhs: Int): Int =
        lhs max rhs
    }

  val minInt =
    new Monoid[Int] {
      def empty: Int = Int.MaxValue
      def combine(lhs: Int, rhs: Int): Int =
        lhs min rhs
    }

  // string
  // List[Char]
  implicit val stringConcat: Monoid[String] =
    new Monoid[String] {
      def empty: String = ""
      // "" + "a" = "a"
      // "a" + "" = "a"
      def combine(lhs: String, rhs: String): String =
        lhs + rhs
      // ("a" + "b") + "c" = "a" + ("b" + "c")
      // ("a" + "b") + "c" = "a" + ("b" + "c")
    }

  // list
  implicit val listConcat: Monoid[List[Int]] =
    new Monoid[List[Int]] {
      def empty: List[Int] = List.empty
      def combine(lhs: List[Int], rhs: List[Int]): List[Int] =
        lhs ++ rhs
    }

  // map monoid
  implicit def mapConcat[K, V](implicit valueMonoid: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def empty: Map[K, V] = Map.empty

      def combine(lhs: Map[K, V], rhs: Map[K, V]): Map[K, V] =
        lhs.foldLeft(rhs) { case (acc, (k, v)) =>
          acc.updated(
            k,
            valueMonoid.combine(v, acc.getOrElse(k, valueMonoid.empty))
          )
        }
    }

  // function monoid
  implicit def functionMonoid[A]: Monoid[A => A] =
    new Monoid[A => A] {
      def empty: A => A = identity
      def combine(lhs: A => A, rhs: A => A): A => A =
        lhs andThen rhs
    }

  final case class Pair[A](a1: A, a2: A) {
    def combine(implicit monoid: Monoid[A]): A =
      monoid.combine(a1, a2)
  }

  val intPair    = Pair(1, 2)
  val stringPair = Pair("a", "b")
  val listPair   = Pair(List(1, 2), List(3, 4))
  println(intPair.combine)
  println(stringPair.combine)
  println(listPair.combine)

  def summarize[A](as: List[A])(implicit monoid: Monoid[A]): A =
    as match {
      case a :: as => monoid.combine(a, summarize(as))
      case Nil     => monoid.empty
    }

  def concatAsMonoid[A](as: List[A])(implicit monoid: Monoid[A]): A =
    as.foldLeft(monoid.empty)(monoid.combine)

  val ints: List[Int]      = List(1, 2, 3)
  val string: List[String] = List("a", "b", "c")
  val bools                = List(true, false, true)

  val listOfListsOfInts: List[List[Int]] =
    List(List(1, 2), List(3, 4), List(5, 6))

  val mapOfScores1 = Map("kit" -> 2, "kat" -> 3, "kitty" -> 4)
  val mapOfScores2 = Map("kit" -> 100, "kat" -> 4, "kaboom" -> 5)
  val mapOfMapOfStrings1 =
    Map("a" -> Map("b" -> "ccc", "d" -> "e"), "f" -> Map("g" -> "h"))
  val mapOfMapOfStrings2: Map[String, Map[String, String]] =
    Map("a" -> Map("b" -> "c", "d" -> "e"), "f" -> Map("h" -> "i"))

  import MonoidSyntax._

  println( //
    mapOfMapOfStrings1 |+| mapOfMapOfStrings2
  )
}

object MonoidSyntax {
  implicit class MonoidOps[A](a: A) {
    def |+|(that: A)(implicit monoid: Monoid[A]): A =
      monoid.combine(a, that)
  }
}

// things that might saved in a db
// things that might be sent over the wire

//trait Identity[A] extend BaseIdentity[A]{
//  def identifier: String =
//  def externalIdentifer: String
//}

//trait UserRepository  {
//  def getUsers: Task[List[User]]
//}
//
//final case class UserRepositoryLive(database: Database) extends UserRepository {
//  def getUsers: Task[List[User]] =
//    Task {
//      database.getUsers
//    }
//}

// Would you be able to talk about when to use different types of polymorphism?
//
// Subtype    Polymorphism (Inheritance)
// Parametric Polymorphism (Generics)
// Ad-Hoc     Polymorphism (Type Classes)
// ADTs

// ZIO <- concurrent
// effect1 zipPar effectTwo

// Cats Effect
// trait Sync[F[_]] {
// trait MonadicFail[F[_]] {
// trait Async[F[_]] extends Sync[F] {
// F[Int] <*>

final case class Box[A](value: A) {
  //
}

final case class Num(n: Int) {
  def add(that: Num): Num =
    Num(this.n + that.n)

}

object Num {
  val zero: Num = Num(0)

  // encoders/decoders pure boilerplate
}

object AdHocPolymorphism extends App {
  // Iso morphism
  // Poly-morphism
  // msum(nums)
  Num.zero // <==
//  Monoid[Num].empty

  trait Increaser[A] {
    def increase(a: A): A
  }

  //  def increase(int: Int): Int              = int + 1
  //  def increase(string: String): String     = string + "!"
  //  def increase(list: List[Int]): List[Int] = list.map(_ + 1)
  object Increaser {
    implicit val increaseInt = new Increaser[Int] {
      def increase(a: Int): Int = a + 1
    }
    implicit val increaseString = new Increaser[String] {
      def increase(a: String): String = a + "!"
    }
//    implicit val increaseList = new Increaser[List[Int]] {
//      def increase(a: List[Int]): List[Int] = a.map(_ + 1)
//    }
    implicit def increaseList[A: Increaser]: Increaser[List[A]] =
      new Increaser[List[A]] {
        def increase(a: List[A]): List[A] =
          a.map(implicitly[Increaser[A]].increase)
      }
  }

  def increase[A](a: A)(implicit increaser: Increaser[A]): A =
    increaser.increase(a)

  println(increase(1))
  println(increase("a"))
  println(increase(List("a", "b", "c")))

}

// Functor | Monad
// F[_]
// List[_] Option[_]
// map()
//
//
//
//
//
//
//
//
//
