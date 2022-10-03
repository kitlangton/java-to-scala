package course.archive.cohort3

import pprint.pprintln

object Day7 {}

object Variance extends App {
  // subtyping
  // if A <: B then we can use A wherever we can use a B
  // if A <: B then A is MORE SPECIFIC than B

  trait Food
  trait Donut extends Food {
    override def toString: String = "DONUT"
    def hasSprinkles: Boolean     = true
  }
  trait Pasta extends Food {
    override def toString: String = "PASTA"
    def hasBolognese: Boolean     = true
  }
  val donut: Donut    = new Donut {}
  val donutFood: Food = donut

  // Any Container/Producer of A
  // - map
  // - zip
  // Fallible Container/Producer of A
  // - orElse
  sealed trait Option[+A] extends Product with Serializable { self =>
    import Option._

    // A :> Any
    // B :> Any
    // B is a supertype of A
    // subtyping
    def getOrElse[A1 >: A](default: A1): A1 = self match {
      case Some(value) => value
      case None        => default
    }
  }

  object Option {
    final case class Some[A](value: A) extends Option[A]
    case object None                   extends Option[Nothing]
    def apply[A](value: A): Option[A] = Some(value)
  }

  val intOption: Option[Int] = Option.apply(12)
  val result: Any            = intOption.getOrElse(12)
  println(result)

  final case class Box[+A](value: A) {}

  val donutBox: Box[Donut]    = Box(donut)
  val donutFoodBox: Box[Food] = donutBox

//  array(0).hasSprinkles
//  class Vessel[+A](var value: A) {
//    def setValue(a: A): Unit = ???
//  }
//  val donutVessel: Vessel[Donut]    = new Vessel(donut)
//  val donutFoodVessel: Vessel[Food] = donutVessel.asInstanceOf[Vessel[Food]]
//  donutFoodVessel.value = new Pasta {}
//  println(donutVessel.value.hasSprinkles)

  // + is Covariant
  // A <: B
  // Box[A] <: Box[B]
  //
  // default is Invariant
  // A <: B
  // Box[A] =!= Box[B]

}

// - Variance
// - Either
// - Parser
// - Implicit Conversions

object BasicSealedTraitExercise extends App {
  // Paste your code hereobject SimpleSealedTraitMethod extends App
  // INSTRUCTIONS
  // ============
  // Complete the isRed method

  sealed trait Color

  object Color {
    case object Red        extends Color
    case object Green      extends Color
    case object Blue       extends Color
    case object Chartreuse extends Color
  }

  def isRed(color: Color): Boolean = color match {
    case Color.Red => true
    case _         => false
  }

  assert(isRed(Color.Red))
  assert(!isRed(Color.Green))
  assert(!isRed(Color.Blue))

}

object PatternMatching extends App {
  // INSTRUCTIONS
  // ============
  // Implement the scan method using pattern matching

  final case class Puppy(name: String, isWearingHat: Boolean)
  final case class Box(contents: Any)

  sealed trait ScanResult
  object ScanResult {
    case object IsString extends ScanResult
    case object IsInt    extends ScanResult
    case object IsPuppy  extends ScanResult
    case object IsOther  extends ScanResult
  }

  def scan(box: Box): ScanResult = box match {
    case Box(_: String) => ScanResult.IsString
    case Box(_: Int)    => ScanResult.IsInt
    case Box(_: Puppy)  => ScanResult.IsPuppy
    case _              => ScanResult.IsOther
  }

  assert(scan(Box("Hello")) == ScanResult.IsOther, "WRONG!")
  assert(scan(Box(42)) == ScanResult.IsInt)
  assert(scan(Box(Puppy("Spot", true))) == ScanResult.IsPuppy)
  assert(scan(Box(42.0)) == ScanResult.IsOther)

}

object CovariantPatterns extends App {

  def trivialRule1[A, B](a: A, aToB: A => B): B = aToB(a)
  def trivialRule2[A, B](a: A, b: B): (A, B)    = (a, b)

  def trivialRule3[A, B, C](a: A, b: B)(abToC: (A, B) => C): C =
    abToC(a, b)

  // map and zip
  // container A -> A
  final case class Box[+A](value: A) { self =>
    def map[B](f: A => B): Box[B]         = Box(f(value))
    def zip[B](that: Box[B]): Box[(A, B)] = Box((value, that.value))

    def zipWith[B, C](that: Box[B])(f: (A, B) => C): Box[C] =
      Box(f(value, that.value))
//      (self zip that).map { case (a, b) => f(a, b) }

    def flatMap[B](f: A => Box[B]): Box[B] =
      f(value)
//      Box.flatten(self.map(f))
  }

  val nested: Box[Box[Int]] = Box(Box(42))
  val flattened: Box[Int]   = Box.flatten(nested)

  object Box {
    def flatten[A](nested: Box[Box[A]]): Box[A] =
      nested.value
  }

  final case class Producer[+A](get: () => A) { self =>
    def map[B](f: A => B): Producer[B] = Producer[B](() => f(get()))

    def zip[B](that: Producer[B]): Producer[(A, B)] =
      Producer(() => (get(), that.get()))

    def zipWith[B, C](that: Producer[B])(f: (A, B) => C): Producer[C] =
      Producer(() => f(get(), that.get()))
  }
//
//  val stringProducer = Producer(() => "Hello")
//  val intProducer    = Producer(() => 42)
//  val zipped: Producer[String] =
//    stringProducer.zipWith(intProducer) { case (str, int) =>
//      str * int
//    }
//  println(zipped.get())

}

object CustomOption extends App {
  // option gives us the powers of safe fallibility
  sealed trait Option[+A] extends Product with Serializable { self =>
    def getOrElse[A1 >: A](default: A1): A1 = self match {
      case Some(value) => value
      case None        => default
    }

    def map[B](f: A => B): Option[B] = self match {
      case Some(a) => Some(f(a))
      case None    => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = self match {
      case Some(a) => f(a)
      case None    => None
    }
  }

  object Option {
    def flatten[A](option: Option[Option[A]]): Option[A] =
      option match {
        case Some(value) => value
        case None        => None
      }
  }

  final case class Some[A](value: A) extends Option[A]
  case object None                   extends Option[Nothing]

  final case class User(id: Int, name: String)

  def getUserUnsafe(id: Int): User =
    if (id == 10)(User(10, "Kimmy"))
    else if (id == 11)(User(11, "Timmy"))
    else throw new Error("MISSING USER")

  def getUser(id: Int): Option[User] =
    if (id == 10) Some(User(10, "Kimmy"))
    else if (id == 11) Some(User(11, "Timmy"))
    else None

  def getEmail(username: String): Option[String] =
    if (username == "Kimmy") Some("kimmy@gmail.com") else None

  def getEmailUnsafe(username: String): String =
    if (username == "Kimmy") "kimmy@gmail.com" else throw new Error("MISSING EMAIL")

  val emailOption: Option[String] =
    Some(10)
      .flatMap { id =>
        getUser(id)
          .flatMap(user =>
            getEmail(user.name)
              .map(email => email)
          )
      }

  // short-circuiting
  val emailOption3: Option[String] =
    for {
      id    <- Some(10)
      user  <- getUser(id)
      email <- getEmail(user.name)
    } yield email

  val email = emailOption3.getOrElse("default")

  val emailOption2: String = {
    val id    = 10
    val user  = getUserUnsafe(id)
    val email = getEmailUnsafe(user.name)
    email
  }

  pprintln(emailOption)
  pprintln(emailOption2)

}

object Pacman extends App {

  def movePacman(stick: Joystick, x: Int, y: Int): (Int, Int) =
    stick match {
      case Joystick.Up     => (x, y + 1)
      case Joystick.Down   => (x, y - 1)
      case Joystick.Left   => (x - 1, y)
      case Joystick.Right  => (x + 1, y)
      case Joystick.Center => (x, y)
    }

  sealed trait Joystick

  object Joystick {
    case object Up     extends Joystick
    case object Down   extends Joystick
    case object Left   extends Joystick
    case object Right  extends Joystick
    case object Center extends Joystick
  }

}
