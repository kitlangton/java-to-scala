package course.cohort4

// - More Type Classes !?
//   - Monoid / Functor / Monad
//   - lots of concrete examples
// - Future / Effect
// - Hackathon (https://shorturl.at/eGHMP)
object Functors extends App {

  // you have own the type to extend them
//  trait Mappable {
//    // Container[A]
//    def map[B](f: A => B): Mapbb
//  }

  // concrete examples
  // Container[B]
  // Container[A] => Container[B]
  // Container[A] => Container[B]
  // Container[A] => Container[B]
//  final case class Mappable(
//      map: (Container[A], A => B) => Container[B]
//                           )
//  trait Mappable[Container[_]] {
//    def map[A, B](containerA: Container[A])(aToB: A => B): Container[B]
//  }
  trait Functor[F[_]] {
    def map[A, B](containerA: F[A])(aToB: A => B): F[B]
  }

  implicit val listMappable: Functor[List] =
    new Functor[List] {
      override def map[A, B](list: List[A])(aToB: A => B): List[B] =
        list.map(aToB)
    }

  implicit val optionMappable: Functor[Option] = new Functor[Option] {
    override def map[A, B](option: Option[A])(aToB: A => B): Option[B] =
      option.map(aToB)
  }

  implicit class FunctorOps[F[_], A](val self: F[A]) {
    def fmap[B](f: A => B)(implicit functor: Functor[F]): F[B] =
      functor.map(self)(f)
  }

  // Functor[F]
  // doSomethingCoolWithFunctor
  // doSomethingCoolWithFunctor
  // doSomethingCoolWithFunctor
  // doSomethingCoolWithFunctor

  // Functor (Applicative) Functor Monad
  // Functor     -> map
  // Applicative -> zip
  // Monad       -> flatMap

  // containers / producers
  // a: A
  // f: A => B
  // f(a): B

  // a : A
  // b : B
  // (A, B)

  // fallible: might have an A or Not
  // orElse
  // None orElse None orElse Some("hello")

  // producer

  // ADTs
  // type classes
  // JsonCodec/circe/zio-json

  // container
  // Option    - fallible
  // Either    - exception
  // Future[A] - concurrency/async
  // Thunk     - suspend
  // ZIO/CIO   - suspend
  // consumers
  // predicate
  final case class Box[A](value: A) {
    def map[B](f: A => B): Box[B] =
      Box(f(value))

    def zip[B](that: Box[B]): Box[(A, B)] =
      Box((value, that.value))

    def flatMap[B](f: A => Box[B]): Box[B] = {
      val result: Box[Box[B]] = map(f)
      Box.flatten(result)
    }
  }

  object Box {
    def flatten[A](boxOfBox: Box[Box[A]]): Box[A] =
      boxOfBox.value
  }
  // map ? reach inside change the value
  // map ? reach inside change the value

  val ints: List[Int] = List(1, 2, 3)

  val strings: List[String] = ints.map(_.toString)

  val maybeUsername: Option[String] = Some("username")
  //                                  Some(8)
  val maybeUsernameLength: Option[Int] = optionMappable.map(maybeUsername)(_.length)

  val eitherErrorPort: Either[String, Int] = Right(1000)
  //                                         Right(true)
  val isPortEven: Either[String, Boolean] = eitherErrorPort.map(_ % 2 == 0)
}

object Day10 {

  // OOP inheritance (traits)
  // - interface for instances
  // - interface for instances and values

  trait Increasable {
    def increase: Increasable
  }

  Coord.increase(Coord(1, 2))

  trait Increase[A] {
    def increase(a: A): A
    def zero: A
  }

  final case class IncreaseBundle[A](increase: A => A, zero: A)

  val coordIncrease: IncreaseBundle[Coord] =
    IncreaseBundle(coord => Coord(coord.x + 1, coord.y + 1), Coord(0, 0))

  coordIncrease.increase(Coord(1, 2))

  final case class Coord(x: Int, y: Int)

  object Coord extends Increase[Coord] {
    def increase(coord: Coord): Coord = Coord(coord.x + 1, coord.y + 1)
    def zero: Coord                   = Coord(0, 0)
  }

  trait JsonDecoder[A] {
    def decode(json: String): A
  }

  final case class JsonDecoderCC[A](
      decode: String => A
  )

  // case class
  // companion object
  trait Greetable {
    def greet: String
  }

  final case class Dog(name: String) extends Greetable {
    def greet: String = s"Woof! I'm ${this.name}"
  }

  object Dog {
    def greet(dog: Dog): String = s"Woof! I'm ${dog.name}"

    def goodBoy: Dog = Dog("Spotty")
  }

  Dog("Fido").greet
  Dog.goodBoy

  final case class Human(name: String) extends Greetable {
    def greet: String = s"Hello! I'm $name"
  }
}

final case class Thunk[A](run: () => A) {
  def map[B](f: A => B): Thunk[B] =
    Thunk { () =>
      f(run())
    }

  def zip[B](that: Thunk[B]): Thunk[(A, B)] =
    Thunk { () =>
      (run(), that.run())
    }

  def flatMap[B](f: A => Thunk[B]): Thunk[B] =
    Thunk { () =>
      f(run()).run()
    }
}

object Thunk extends App {
  val int = Thunk { () =>
    println("computing int")
    1
  }

  val thunk: Thunk[Int] =
    for {
      x <- int
      y <- int
      z <- int
    } yield x + y + z

  println(thunk.run())
}
