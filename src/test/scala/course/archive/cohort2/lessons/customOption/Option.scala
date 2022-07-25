package course.lessons.customOption

import Option._

// Parameterized Containers
// ========================
//
// Container(a)
//
//
// List[A]
// Option[A]
// Pair[A]
//               A  <:     B
// Covariant Box[A] <: Box[B]

sealed trait Option[+A] extends Product with Serializable {

  // Cat
  def getOrElse[A1 >: A](default: => A1): A1 =
    this match {
      case Some(value) => value
      case None        => default
    }

  def map[B](f: A => B): Option[B] =
    this match {
      case Some(a) => Some(f(a))
      case None    => None
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case Some(a) => f(a)
      case None    => None
    }

  def zip[B](that: Option[B]): Option[(A, B)] =
    (this, that) match {
      case (Some(a), Some(b)) => Some((a, b))
      case _                  => None
    }

}

object Option {
  final case class Some[+A](value: A) extends Option[A]
  case object None                    extends Option[Nothing]

  def flatten[A](option: Option[Option[A]]): Option[A] =
    option match {
      case Some(value) => value
      case None        => None
    }
}

final case class Box[+A](value: A)

object Box {
  def flatten[A](nested: Box[Box[A]]): Box[A] =
    nested.value
}

object OptionExamples extends App {

  trait Animal

  final case class Dog(name: String) extends Animal
  final case class Cat(lives: Int)   extends Animal

  val dogOption: Option[Dog] = Some(Dog("Fido"))

  val animalOption: Animal =
    dogOption.getOrElse(Cat(5))

  val optionString: Option[String] = Some("Hello")
  val optionInt: Option[Int]       = optionString.map((string: String) => string.length)

  final case class Address(street: Int, country: Option[String])
  final case class Person(name: String, address: Option[Address])

  val maybePerson: Option[Person] =
    Some(Person("John", None))

  val maybeCountry1: Option[String] =
    maybePerson
      .flatMap(_.address)
      .flatMap(_.country)

  // Future[A]
  val maybeCountry2: Option[String] =
    for {
      person  <- maybePerson
      address <- person.address
      country <- address.country
    } yield country

  println(maybeCountry1)

}
