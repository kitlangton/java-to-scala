package course.cohort4

// DAY 9!!!
object Day9 extends App {}

sealed trait Toggle

object Toggle {
  case object On  extends Toggle
  case object Off extends Toggle
}

sealed trait Aliveness

object Aliveness {
  case object IsAlive extends Aliveness
  case object IsDead  extends Aliveness
}

object ExampleToggle extends App {
  def useToggle(toggle: Toggle) = ???

  def confusingMethod(toggle: Toggle, shouldExist: Boolean, isAlive: Aliveness) =
    useToggle(toggle)
}

object EitherTests extends App {
  sealed trait Either[+E, +A]

  object Either {
    case class Right[A](value: A) extends Either[Nothing, A]

    case class Left[E](error: E) extends Either[E, Nothing]
  }

  sealed trait DatabaseError

  object DatabaseError {
    case object ConnectionFailed extends DatabaseError

    case class SQLError(code: Int, message: String) extends DatabaseError
  }

  final case class User(name: String)

  def getUser(id: Int): Either[DatabaseError, User] = ???

  getUser(123) match {
    case Either.Right(user)                                 => println(user)
    case Either.Left(DatabaseError.ConnectionFailed)        => println("Connection failed")
    case Either.Left(DatabaseError.SQLError(code, message)) => println(s"SQL error $code: $message")
  }

}

object Isomorphisms extends App {

  final case class Isomorphism[A, B](
      to: A => B,
      from: B => A
  ) {
    def identityA(a: A): A = from(to(a))
    def identityB(b: B): B = to(from(b))
  }

  sealed trait Toggle extends Product with Serializable
  case object On      extends Toggle
  case object Off     extends Toggle

  val isoToggleBoolean = Isomorphism[Toggle, Boolean](
    to = {
      case On  => true
      case Off => false
    },
    from = {
      case true  => On
      case false => Off
    }
  )

  def isoTest[A, B](isomorphism: Isomorphism[A, B])(as: List[A], bs: List[B]): Unit = {
    val roundTripped = as.map(isomorphism.identityA)
    val isEqual      = roundTripped == as
    println(s"as = $as")
    println(s"ra = $roundTripped")
    if (isEqual) println("As Success!") else println("As Failure!")

    val roundTripped2 = bs.map(isomorphism.identityB)
    val isEqual2      = roundTripped2 == bs
    println(s"bs = $bs")
    println(s"rb = $roundTripped2")
    if (isEqual2) println("Bs Success!") else println("Bs Failure!")
  }

  isoTest(isoToggleBoolean)(List(On, Off), List(true, false))

  // (A, B) =~ (B, A)

  val isoTuple = Isomorphism[(Int, String), (String, Int)](
    to = _.swap,
    from = _.swap
  )

  // A + B + C
  sealed trait ThreeBooleans extends Product with Serializable

  // 6 = 1 + 1 + 1 + 1 + 1 + 1
  // 6 = 3 + 3
  // 6 = 3 * 2

  // 2 + 2 + 2
  object ThreeBooleans {
    final case class Boolean1(b: Boolean) extends ThreeBooleans
    final case class Boolean2(b: Boolean) extends ThreeBooleans
    final case class Boolean3(b: Boolean) extends ThreeBooleans
  }
  val b1 = ThreeBooleans.Boolean1(true)
  val b2 = ThreeBooleans.Boolean1(false)
  val b3 = ThreeBooleans.Boolean2(true)
  val b4 = ThreeBooleans.Boolean2(false)
  val b5 = ThreeBooleans.Boolean3(true)
  val b6 = ThreeBooleans.Boolean3(false)

  // 2 * 2 * 2 = 8
  final case class ProductThreeBooleans(b1: Boolean, b2: Boolean, b3: Boolean)
  val p1 = ProductThreeBooleans(true, true, true)
  val p2 = ProductThreeBooleans(true, true, false)
  val p3 = ProductThreeBooleans(true, false, true)
  val p4 = ProductThreeBooleans(true, false, false)
  val p5 = ProductThreeBooleans(false, true, true)
  val p6 = ProductThreeBooleans(false, true, false)
  val p7 = ProductThreeBooleans(false, false, true)
  val p8 = ProductThreeBooleans(false, false, false)

  // Unit    = 1
  // Nothing = 0
  // 2 * 0 = 0
  final case class BooleanNothing(b: Boolean, n: Nothing)
  BooleanNothing(true, ???)
  // 1 + 0 = 1
  sealed trait OnePlusZero extends Product with Serializable
  object OnePlusZero {
    final case object One               extends OnePlusZero
    case class Naught(nothing: Nothing) extends OnePlusZero
  }
  OnePlusZero.One //1
//  OnePlusZero.Naught(???)
}

///
///
///
///
///
///
///
///
///
///
///
///
///
///
///
///
///
///
///
///

///
