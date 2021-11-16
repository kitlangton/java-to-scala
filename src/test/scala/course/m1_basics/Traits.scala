package course.m1_basics

import course.Lesson
import zio.test.TestAspect.ignore
import zio.test.assertTrue

/** TRAITS
  *
  *   - Simple traits
  *   - Using new to create an instance
  *   - Extending a trait
  *   - Extending multiple traits
  *
  * Traits are tool for creating abstractions in Scala, similar to Java
  * interfaces. They're used to define a set of related definitions, without
  * requiring implementations.
  */
object Traits extends Lesson {

  // A trait definition
  trait Pokeable {

    // A method without a body
    def poke: String
  }

  /** You can can create an instance of a trait by using the new keyword
    * followed by the name of the trait. You will then have to implement the
    * interface.
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

  case class Triangle(base: Double, height: Double) extends Shape {
    override def area: Double      = base * height / 2
    override def perimeter: Double = base + height + math.sqrt(base * base + height * height)
  }

  case class Circle(radius: Double) extends Shape {
    override def area: Double      = radius * radius * Math.PI
    override def perimeter: Double = 2 * radius * Math.PI
  }

  // <- EDIT HERE (Create a Rectangle case class that extends Shape)

  val shapeTest = test("Shape") {
    assertTrue(
      // Rectangle(3, 4).area == 12 // <- Uncomment this
      // Rectangle(3, 4).perimeter == 14 // <- Uncomment this
      Triangle(3, 4).area == 6,
      Triangle(3, 4).perimeter == 12,
      Circle(3).area == 28.274333882308138,
      Circle(3).perimeter == 18.84955592153876
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
    val triangle = Triangle(3, 4)
    val circle   = Circle(3)
    assertTrue(
      // areaTimesPerimeter(triangle) == 72 <-- Uncomment this
      // areaTimesPerimeter(circle) == 532.9586376588253 <-- Uncomment this
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
    val name: String
  }

  final case class Celebrity(name: String) extends Identifiable with Pokeable {
    def poke: String = s"Do you know who you're poking? You're poking $name!"
  }

  def exercise =
    suite("Traits")(
      pokeableTest,
      customPokeableTest,
      shapeTest,
      subtypingTest
    )
}
