package course.lessons.projects

// A => Boolean
object Predicate extends App {

  trait Animal {
    def name: String
  }
  case class Cat(name: String, lives: Int)    extends Animal
  case class Dog(name: String, bone: Boolean) extends Animal

  //   A <: B
  // + F[A] <: F[B]
  //   F[B] <: F[A]
  // narrow on the way in -A => +B widen on the way out
  val animalPredicate: Predicate[Animal] = Predicate(animal => animal.name == "Fido")
  val dogPredicate: Predicate[Dog]       = animalPredicate

  dogPredicate.run(Dog("Fido", true))

  // COVARIANT: Producer or Container of A's
  // CONTRAVARIANT: Consumer or Processor of A's
  final case class Predicate[-A](run: A => Boolean) {
    def ||[A1 <: A](that: Predicate[A1]): Predicate[A1] =
      or(this, that)

    def &&[A1 <: A](that: Predicate[A1]): Predicate[A1] =
      and(this, that)
  }

  // constructors -> minimal solution to the smallest "problems"
  def equalTo[A](value: A): Predicate[A] =
    Predicate((a: A) => a == value)

  def lessThan(max: Int): Predicate[Int] =
    Predicate((int: Int) => int < max)

  // combinators -> combine sub-solutions into bigger solutions
  def and[A](left: Predicate[A], right: Predicate[A]): Predicate[A] =
    Predicate((a: A) => left.run(a) && right.run(a))

  def or[A](left: Predicate[A], right: Predicate[A]): Predicate[A] =
    Predicate((a: A) => left.run(a) || right.run(a))

  def not[A](predicate: Predicate[A]): Predicate[A] =
    Predicate((a: A) => !predicate.run(a))

  val equalTo100: Predicate[Int] = equalTo(100)
  val lessThan50: Predicate[Int] = lessThan(50)
  val composed                   = equalTo100 || lessThan50
  println(composed.run(100)) // true
  println(composed.run(49))  // true
  println(composed.run(55))  // false
}
