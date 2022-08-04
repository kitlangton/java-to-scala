package course.cohort3

import pprint.pprintln

object Day4 {}

case class Cat(name: String, mood: Double, numLives: Int = 9)

object CaseClassCat extends App {
  // INSTRUCTIONS
  // ============
  // Define a method or two on the Cat case class that will modify it somehow, returning a new Cat.

  case class Cat(name: String, mood: Double, numLives: Int = 9) {
    def newCat(newName: String = name, newMood: Double = mood, lives: Int = numLives): Cat =
      Cat(newName, newMood, lives)

    def roadKill: Cat = copy(numLives = numLives - 1)
  }

  val garfield = Cat("Garfield", 0.9)
  val copied   = garfield.roadKill
  pprintln(garfield)
  pprintln(copied)

}

case class Cat2(name: String, mood: Double, numLives: Int = 9) {
  def changeName(newName: String): Cat2 = this.copy(name = newName)
}

object GenericMethodsTest extends App {
  def intBaton(int: Int): Int             = int
  def stringBaton(string: String): String = string
  def catBaton(cat: Cat): Cat             = cat

  final case class IntBox(value: Int)
  final case class StringBox(value: String)
  final case class CatBox(value: Cat)

  def extractInt(box: IntBox): Int          = box.value
  def extractString(box: StringBox): String = box.value
  def extractCat(box: CatBox): Cat          = box.value

  def delegate[Input, Output]( //
      value: Input,
      transform: Input => Output
  ): Output = ???

  def pipeline[Start, Middle, End](
      startToMiddle: Start => Middle,
      middleToEnd: Middle => End
  ): Start => End =
    ???
}

object CaseClassCat2 extends App {
  // INSTRUCTIONS
  // ============
  // Define a method or two on the Cat case class that will modify it somehow, returning a new Cat.

  case class Cat(name: String, mood: Double, numLives: Int = 9) {
    def lostLife(n: Int = 1): Cat = copy(mood = mood / 2, numLives = numLives - n)
  }

  val myCat    = Cat("Feisty", 9)
  val myNewCat = myCat.lostLife(-5)

  pprintln(myCat)
  pprintln(myNewCat)
}

object CaseClassCat3 extends App {

  case class Cat(name: String, mood: Double, numLives: Int = 9) {
    def hungry: Cat = copy(mood = 2)
  }

//  val cat = Cat("kitty", 1)
//  pprintln(cat.hungry)
  def genericBaton[A](value: A): A => A = (input: A) => input

//  def genericBaton[A](value: A): A => A = (input: A) => input
  def identity[A](a: A) = a
//  val result: Int                       = genericBaton(123)(5)
  pprintln(identity(123))

}

object WeirdStuff extends App {

  final case class IntBox(value: Int)
  final case class StringBox(value: String)
  final case class CatBox(value: Cat)
  final case class ABox[A](value: A)

  def extractInt(box: ABox[Int]): Int          = box.value
  def extractString(box: ABox[String]): String = box.value
  def extractCat(box: ABox[Cat]): Cat          = box.value

  def extractABox[A](box: ABox[A]): A = box.value

  pprintln(extractABox(ABox("HELLO")))

}

object ParametricPolymorphismPracticePartTwo extends App {
  // INSTRUCTIONS
  // ============
  // Factor out the duplication in the following case classes and their corresponding methods by using generics.

  final case class IntBox(value: Int)
  final case class Box[A](value: A)
  final case class AnotherBox[A](value: A)
  def extractGeneric[A](box: Box[A]): A = box.value
  val intBox: Box[Int]                  = Box(10)
  println(extractGeneric(intBox))

}

object WeirdFunctionsOne extends App {

  // Input = String
  // a,b,c -> generic values
  // as, bs, cs -> generic lists (as: List[A])
  // f,g,h -> functions of some sort
  // p -> predicate
//  def delegate[A, B](a: A)(f: A => B): B = f(a)

  def delegate[Input, Output](value: Input)(transform: Input => Output): Output =
    transform(value)

  pprintln(delegate("world")(value => value))

}

// Code Style / Scala
// format on save
// code styling tool scalafmt
object Another extends App {
  def double(int: Int): Int = int * 2

  def delegate[A, B](a: A, f: A => B): B = f(a)

  pprintln(delegate(2, double))
}

trait MyFunction[A, B] { self =>
  def apply(input: A): B
  def andThen[C](g: MyFunction[B, C]): MyFunction[A, C] = (a: A) => g(self(a))
}

object AndThenExample extends App {
  val stringLength: MyFunction[String, Int] = (input: String) => input.length
  val isEven: MyFunction[Int, Boolean]      = (input: Int) => input % 2 == 0
  val composed                              = stringLength.andThen(isEven)
  pprintln(composed.apply("hello!"))
}
