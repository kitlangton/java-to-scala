package course.archive.cohort1.miscellany.fancy

import course.archive.cohort2.exercises.Exercise
import zio.test.TestAspect.ignore
import zio.test._

/** ISOMORPHISM
  *
  * A type is a set of discrete values. For any type, you could technically
  * enumerate each value in a type. This is trivial to do for Boolean, as it is
  * so small. This becomes more labor intensive as types get larger, doubly so
  * for infinite types like `List` or `String`.
  */
object L1_Isomorphism extends Exercise {
  import Toggle._

  /** Here is the definition of a new type, `Toggle`, which has 2 values: `On`
    * and `Off`. Because `Toggle` has the same cardinality as `Boolean`, we can
    * prove that they are _isomorphic_ by creating invertible mappings between
    * the two types.
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

  val testMapsToBoolean =
    test("Toggle -> Boolean") {
      assertTrue(
        toggleToBoolean(On) == true,
        toggleToBoolean(Off) == false
      )
    } @@ ignore

  // A mapping from `Boolean` to `Toggle`
  def booleanToToggle(boolean: Boolean): Toggle =
    if (boolean) On else Off

  val testMapsFromBoolean =
    test("Boolean -> Toggle") {
      assertTrue(
        booleanToToggle(true) == On,
        booleanToToggle(false) == Off
      )
    } @@ ignore

  val roundTripping =
    test("Boolean <-> Toggle") {
      assertTrue(
        toggleToBoolean(booleanToToggle(true)) == true,
        toggleToBoolean(booleanToToggle(false)) == false,
        booleanToToggle(toggleToBoolean(On)) == On,
        booleanToToggle(toggleToBoolean(Off)) == Off
      )
    } @@ ignore

  def exercise =
    suite("Isomorphism")(
      testMapsToBoolean,
      testMapsFromBoolean,
      roundTripping
    )
}
