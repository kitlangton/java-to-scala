package course.exercises

import zio.test.TestAspect.ignore
import zio.test._

//  ██████╗ █████╗ ██████╗ ██████╗ ██╗███╗   ██╗ █████╗ ██╗     ██╗████████╗██╗   ██╗
// ██╔════╝██╔══██╗██╔══██╗██╔══██╗██║████╗  ██║██╔══██╗██║     ██║╚══██╔══╝╚██╗ ██╔╝
// ██║     ███████║██████╔╝██║  ██║██║██╔██╗ ██║███████║██║     ██║   ██║    ╚████╔╝
// ██║     ██╔══██║██╔══██╗██║  ██║██║██║╚██╗██║██╔══██║██║     ██║   ██║     ╚██╔╝
// ╚██████╗██║  ██║██║  ██║██████╔╝██║██║ ╚████║██║  ██║███████╗██║   ██║      ██║
//  ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝╚═════╝ ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝╚══════╝╚═╝   ╚═╝      ╚═╝

/** Here we'll ply our knowledge of the "Algebraic" part of Algebraic Data
  * Types, and calculate how many values are representable by different ADTs.
  * The technical term for the-size-of-a-type is Cardinality. So, in other
  * words, we'll be counting cardinalities.
  *
  * Remember that sealed traits (i.e., Sum Types) perform type-level addition,
  * whereas case classes (i.e., Product Types) perform type-level
  * multiplication. Type parameters create type-level functions of sorts,
  * allowing us to substitute in (or "apply") the cardinality of another type.
  *
  * If the following formulas don't seem too frightening, then you'll be able to
  * handle the exercises no problem. ;-)
  *   - 8
  *   - 2 + 2
  *   - 5 * 5
  *   - (2 + 3) * 2
  *   - x * 3
  *   - x + y
  */

object E13_Cardinality extends Exercise {

  /** ✏ EXERCISE
    *
    * List every possible Boolean value.
    */
  val allValues: Set[Boolean] = Set() // <- Add every possible Boolean value, separated by commas

  val testAllBooleanValues =
    test("all Boolean values") {
      assertTrue(allValues == Answers.allBooleanValues)
    } @@ ignore

  /** CARDINALITY
    *
    * The cardinality of a set (or type) is simply its size.
    *
    * ✏ EXERCISE
    *
    * Change the value from 0 to the actual cardinality of `Boolean`.
    */
  val booleanCardinality: Int = 0 // <- Update the cardinality here

  val testBoolean =
    test("Boolean cardinality") {
      assertTrue(booleanCardinality == Answers.booleanCardinality)
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Identify the correct cardinality for the `TheThing` case object.
    */
  case object TheThing

  val theThingCardinality: Int = 0 // <- Update the cardinality here

  val testTheThing =
    test("TheThing cardinality") {
      assertTrue(theThingCardinality == Answers.theThingCardinality)
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Identify the correct cardinality for the `RPS` sealed trait.
    */
  sealed trait RPS

  object RPS {
    case object Rock     extends RPS
    case object Paper    extends RPS
    case object Scissors extends RPS
  }

  val rpsCardinality: Int = 0 // <- Update the cardinality here

  val testRPS =
    test("RPS cardinality") {
      assertTrue(rpsCardinality == Answers.rpsCardinality)
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Identify the correct cardinality for the `RPSGame` case class.
    */
  final case class RPSRound(playerOne: RPS, playerTwo: RPS)

  val rpsRoundCardinality: Int = 0 // <- Update the cardinality here

  val testRPSRound =
    test("RPS cardinality") {
      import RPS._
      val moves     = List(Rock, Paper, Scissors)
      val allRounds = for { m1 <- moves; m2 <- moves } yield RPSRound(m1, m2)

      assertTrue(rpsRoundCardinality == allRounds.length)
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Identify the correct cardinality for the `RPSThumb` sealed trait.
    */
  sealed trait RPSThumb

  object RPSThumb {
    final case class RegularMove(move: RPS)         extends RPSThumb
    final case class Thumb(thumbPointedUp: Boolean) extends RPSThumb
  }

  val rpsThumbCardinality: Int = 0 // <- Update the cardinality here

  val testRPSThumb =
    test("RPSThumb cardinality") {
      assertTrue(rpsThumbCardinality == Answers.rpsThumbCardinality)
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Identify the correct cardinality for the `Box` case class.
    */
  final case class Box[A](content: A, isWrapped: Boolean)

  val boxCardinality: Int = 0 // <- Update the cardinality here

  type BooleanBox = Box[Boolean]
  val booleanBoxCardinality: Int = 0 // <- Update the cardinality here

  type RPSRoundBox = Box[RPSRound]
  val rpsRoundBoxCardinality: Int = 0 // <- Update the cardinality here

  type BooleanBoxBox = Box[Box[Boolean]]
  val booleanBoxBoxCardinality: Int = 0 // <- Update the cardinality here

  val testBox =
    test("Box cardinality") {
      assertTrue(
        booleanBoxCardinality == Answers.booleanBoxCardinality,
        rpsRoundBoxCardinality == Answers.rpsRoundBoxCardinality,
        booleanBoxBoxCardinality == Answers.booleanBoxBoxCardinality
      )
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Identify the correct cardinality for the `RPSThumb` sealed trait.
    */
  sealed trait Option[+A]

  object Option {
    final case class Some[A](value: A) extends Option[A]
    final case object None             extends Option[Nothing]
  }

  val optionCardinality: Int = 0 // <- Update the cardinality here

  // Option[Boolean]
  val optionBooleanCardinality: Int = 0 // <- Update the cardinality here

  // Option[RPS]
  val optionRPSCardinality: Int = 0 // <- Update the cardinality here

  val optionOptionBooleanCardinality: Int = 0 // <- Update the cardinality here

  val optionBoxOptionRPSCardinality: Int = 0 // <- Update the cardinality here

  val testOption =
    test("RPSThumb cardinality") {
      assertTrue(
        optionBooleanCardinality == Answers.optionBooleanCardinality,
        optionRPSCardinality == Answers.optionRPSCardinality,
        optionOptionBooleanCardinality == Answers.optionOptionBooleanCardinality,
        optionBoxOptionRPSCardinality == Answers.optionBoxOptionRPSCardinality
      )
    } @@ ignore

  def exercise =
    suite("Counting")(
      testAllBooleanValues,
      testBoolean,
      testTheThing,
      testRPS,
      testRPSRound,
      testRPSThumb,
      testBox,
      testOption
    )
}
