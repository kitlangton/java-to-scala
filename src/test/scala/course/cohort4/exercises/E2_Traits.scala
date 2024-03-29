package course.cohort4.exercises

import course.archive.cohort2.exercises.Exercise
import zio.test.TestAspect.ignore
import zio.test.assertTrue

/** TRAITS
  *
  * Traits are used for creating abstractions in Scala, similar to Java
  * interfaces. They're used to define a set of related definitions, without
  * requiring implementations.
  */
object E2_Traits extends Exercise {

  // A trait definition
  trait Pokeable {

    // A method without a body
    def poke: String
  }

  /** You can can create an anonymous instance of a trait by using the new
    * keyword followed by the name of the trait. You will then have to implement
    * the interface.
    */
  val doughBoy =
    new Pokeable {
      override def poke: String = "Tee hee!"
    }

  val theKing =
    new Pokeable {
      override def poke: String = "Off with your head!"
    }

  val pokeableTest = test("Pokeable") {
    assertTrue(
      doughBoy.poke == "Tee hee!",
      theKing.poke == "Off with your head!"
    )
  }

  /** ✏ EXERCISE
    *
    * Create your own Pokeable instance.
    */

  lazy val mySpecialPokeable: Pokeable = ??? // <- EDIT HERE

  val customPokeableTest = test("Custom Pokeable") {
    assertTrue(mySpecialPokeable.poke == "Why have you done this?")
  } @@ ignore

  /** ✏ EXERCISE
    *
    * In addition to creating anonymous instances of traits, you can also extend
    * traits in classes, case classes, other traits, etc.
    *
    * Here, we have a Shape trait that defines a common interface for all
    * shapes. Following the pattern, create another Rectangle trait and then
    * uncomment the labeled assertions.
    */

  trait Shape {
    def area: Double

    def perimeter: Double
  }

  class Triangle(base: Double, height: Double) extends Shape {
    override def area: Double = base * height / 2

    override def perimeter: Double = base + height + math.sqrt(base * base + height * height)
  }

  class Circle(radius: Double) extends Shape {
    override def area: Double = radius * radius * Math.PI

    override def perimeter: Double = 2 * radius * Math.PI
  }

  // <- EDIT HERE (Create a Rectangle case class that extends Shape)

  val shapeTest = test("Shape") {
    assertTrue(
      // new Rectangle(3, 4).area == 12, // <- Uncomment this
      // new Rectangle(3, 4).perimeter == 14, // <- Uncomment this
      new Triangle(3, 4).area == 6,
      new Triangle(3, 4).perimeter == 12,
      new Circle(3).area == 28.274333882308138,
      new Circle(3).perimeter == 18.84955592153876
    )
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Types that extend a trait are considered subtypes of are considered
    * subtypes of that trait. You can use subtype, anywhere a supertype is
    * required.
    *
    * Below or two functions, one which operates in a triangle, and another
    * which operates in a circle. Define a new method that takes a Shape,
    * allowing us to remove the duplication. Then, uncomment the assertions.
    */

  def triangleAreaTimesPerimeter(triangle: Triangle): Double =
    triangle.area * triangle.perimeter

  def circleAreaTimesPerimeter(circle: Circle): Double =
    circle.area * circle.perimeter

  def areaTimesPerimeter = ??? // <- EDIT HERE

  val subtypingTest = test("Subtyping") {
    val triangle = new Triangle(3, 4)
    val circle   = new Circle(3)
    assertTrue(
      // areaTimesPerimeter(triangle) == 72, <-- Uncomment this
      // areaTimesPerimeter(circle) == 532.9586376588253, <-- Uncomment this
      triangleAreaTimesPerimeter(triangle) == 72,
      circleAreaTimesPerimeter(circle) == 532.9586376588253
    )
  } @@ ignore

  /** ☃︎ EXAMPLE
    *
    * It is possible to extend multiple traits as well, as long as the behaviors
    * do not conflict.
    */

  trait Identifiable {
    def name: String
  }

  class Celebrity(val name: String) extends Identifiable with Pokeable {
    def poke: String = s"Do you know who you're poking? You're poking $name!"
  }

  /** ☃︎ EXAMPLE
    *
    * It is possible to define default implementations, which may or may not
    * leverage other methods on the trait.
    */

  trait Introducable {
    def name: String

    def introduce(): Unit = println(s"Hi, I'm $name!")
  }

  /** ☃︎ EXERCISE
    *
    * Create the missing constructors for the Iterator trait. This is not a
    * functional construct, but will get us acquainted with polymorphic traits.
    *
    * Quoting the Scala language reference:
    *
    * "Iterators are data structures that allow to iterate over a sequence of
    * elements. They have a hasNext method for checking if there is a next
    * element available, and a next method which returns the next element and
    * advances the iterator."
    *
    * "An iterator is mutable: most operations on it change its state. While it
    * is often used to iterate through the elements of a collection, it can also
    * be used without being backed by any collection"
    */
  trait Iterator[A] {
    def hasNext: Boolean

    def next(): A
  }

  object Iterator {
    def fromArray[A](array: Array[A]): Iterator[A] = ??? // <- EDIT HERE

    def fill[A](n: Int)(element: A): Iterator[A] = ??? // <- EDIT HERE
  }

  val arrayIteratorTest = test("Array Iterator") {
    val array    = Array(1, 2, 3, 4, 5)
    val iterator = Iterator.fromArray(array)
    var sum      = 0
    while (iterator.hasNext)
      sum += iterator.next()
    assertTrue(
      !iterator.hasNext,
      sum == 15
    )
  } @@ ignore

  val fillIteratorTest = test("Fill Iterator") {
    val iterator = Iterator.fill(5)("Hello")
    var size     = 0
    var allHello = true
    while (iterator.hasNext) {
      allHello = allHello && iterator.next() == "Hello"
      size += 1
    }
    assertTrue(
      !iterator.hasNext,
      allHello,
      size == 5
    )
  } @@ ignore

  def exercise =
    suite("Traits")(
      pokeableTest,
      customPokeableTest,
      shapeTest,
      subtypingTest,
      arrayIteratorTest,
      fillIteratorTest
    )
}
