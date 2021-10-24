package course

import zio.test._

import java.lang.Throwable

object Lesson_01_Introduction extends Lesson {

  def exercise =
    /** # TYPES
      *
      * A type is a set of discrete values. For any type, you could technically enumerate each value in a type. This is
      * trivial to do for Boolean, as it is so small. This becomes more labor intensive as types get larger, doubly so
      * for infinite types like `List` or `String`.
      */
    suite("Types")(
      suite("Boolean")(
        /** EXERCISE
          *
          * Enumerate every Boolean value.
          */
        test("all values") {
          val allValues: Set[Boolean] = Set() // <- Write each Boolean value, separated by commas
          assertTrue(allValues == Answers.allBooleanValues)
        },
        /** # CARDINALITY
          *
          * The cardinality of a set (or type) is simply its size.
          *
          * EXERCISE
          *
          * Change the value from 0 to the actual cardinality of `Boolean`.
          */
        test("cardinality") {
          val booleanCardinality: Int = 0 // <- Update the cardinality here
          assertTrue(booleanCardinality == Answers.booleanCardinality)
        }
      ),
      suite("Int") {

        /** EXERCISE
          *
          * Int in Scala is a signed 32 bit number. How many distinct states can an Int represent?
          */
        test("cardinality") {
          val intCardinality: Double = 0 // <- Update the cardinality here

          var actualCardinality: Long = 0
          (Int.MinValue to Int.MaxValue).foreach { _ =>
            actualCardinality += 1
          }

          assertTrue(intCardinality == actualCardinality)
        }
      }
    )
}

/** ISOMORPHISM
  *
  * A type is a set of discrete values. For any type, you could technically enumerate each value in a type. This is
  * trivial to do for Boolean, as it is so small. This becomes more labor intensive as types get larger, doubly so for
  * infinite types like `List` or `String`.
  */
object Isomorphism extends Lesson {
  import Toggle._

  /** Here is the definition of a new type, `Toggle`, which has 2 values: `On` and `Off`. Because `Toggle` has the same
    * cardinality as `Boolean`, we can prove that they are _isomorphic_ by creating invertible mappings between the two
    * types.
    */
  sealed trait Toggle

  object Toggle {
    case object On  extends Toggle
    case object Off extends Toggle
  }

  // A mapping from `Toggle` to `Boolean`
  def toggleToBoolean(toggle: Toggle): Boolean =
    toggle match {
      case On  => true
      case Off => false
    }

  val testMapsToBoolean = test("Toggle -> Boolean") {
    assertTrue(
      toggleToBoolean(On) == true,
      toggleToBoolean(Off) == false
    )
  }

  // A mapping from `Boolean` to `Toggle`
  def booleanToToggle(boolean: Boolean): Toggle =
    if (boolean) On else Off

  val testMapsFromBoolean = test("Boolean -> Toggle") {
    assertTrue(
      booleanToToggle(true) == On,
      booleanToToggle(false) == Off
    )
  }

  val roundTripping = test("Boolean <-> Toggle") {
    assertTrue(
      toggleToBoolean(booleanToToggle(true)) == true,
      toggleToBoolean(booleanToToggle(false)) == false,
      booleanToToggle(toggleToBoolean(On)) == On,
      booleanToToggle(toggleToBoolean(Off)) == Off
    )
  }

  def exercise =
    suite("Isomorphism")(
      testMapsToBoolean,
      testMapsFromBoolean,
      roundTripping
    )
}

object Answers {
  val allBooleanValues   = Set(true, false)
  val booleanCardinality = 2
}
