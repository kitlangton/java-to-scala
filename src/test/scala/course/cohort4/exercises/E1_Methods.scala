package course.cohort4.exercises

import course.archive.cohort2.exercises.Exercise
import zio.test.TestAspect.ignore
import zio.test.{Spec, TestEnvironment, assertTrue}

object E1_Methods extends Exercise {
  // Default Parameters
  // ==================

  /** ✏ EXERCISE
    *
    * Give the `default` parameter a default value of "Anonymous".
    */
  def userNameOrDefault(name: String, default: String): String =
    if (name.isEmpty) default
    else name

  val testDefault =
    test("default") {
      // 💬 When you're ready, remove the default value from the following line.
      assertTrue(userNameOrDefault("", "Steve") == "Anonymous")
    } @@ ignore

  // Named Arguments
  // ===============

  /** ✏ EXERCISE
    *
    * Call the following method with named arguments.
    */
  def outOfOrderSomeFood(
      isDelicious: Boolean,
      price: Double,
      name: String
  ): Unit = println(
    s"The dish $name is ${if (isDelicious) "delicious" else "not delicious"} and costs $price"
  )

  // 💬 Uncomment the following line and get it to work without changing the order of the arguments.
  // outOfOrderSomeFood(true, 555.55, "golden pizza")

  // By-Name Parameters
  // ==================

  /** ✏ EXERCISE
    *
    * Change the following method to accept a by-name parameter such that it
    * doesn't throw an error when called.
    */

  def greet(name: => String): Unit =
    try println(s"Hello, $name!")
    catch {
      case _: Throwable => println("Easy now, pal!")
    }

  // 💬 Uncomment the following line once you've fixed the `greet` method
  // greet(throw new Error("Oh no!"))

  /** ✏ EXERCISE
    *
    * Change the following method so that it doesn't blow up when called, but
    * also does not evaluate its parameter more than once.
    */

  def greetThrice(name: => String): Unit =
    try {
      val name0 = name
      println(s"Hello hello hello, $name0 $name0 $name0!")
    } catch {
      case _: IndexOutOfBoundsException => println("Easy now, pal!")
    }

  var count = 0
  def candyman: String = {
    count += 1
    if (count == 3)
      throw new RuntimeException("🍭👹🍭 THE CANDYMAN CAN!")
    "Candyman"
  }

  // 💬 Uncomment the following two lines once you've fixed the `greetThrice` method
//  greetThrice(candyman)
//  greetThrice(throw new IndexOutOfBoundsException())

  /** ✏ EXERCISE
    *
    * Modify `ifThenElse` so that it only evaluates the intended action given
    * the condition.
    */

  // Call-By-Name Parameter
  // Call-By-Parens Parameter
  def ifThenElse(condition: Boolean, trueAction: => Unit, falseAction: => Unit): Unit =
    if (condition) trueAction else falseAction

  // 💬 Uncomment the following line once you've implemented `ifThenElse`
  ifThenElse(
    condition = false,
    trueAction = println("YOU SHOULD SEE ME"),
    falseAction = println("YOU SHOULD NOT SEE ME")
  )

  // Multiple Parameter Lists
  // ========================

  /** ✏ EXERCISE
    *
    * Change the following method to use multiple parameter lists.
    */

  def labeledAddition(name: String)(x: Int, y: Int = 10)(z: Int): Unit =
    println(s"$name, $x + $y * $z = ${x + y * z}")

  // 💬 Uncomment the following line and then alter `labeledAddition`
  //    such that it works as expected.
  labeledAddition("WOW")(1)(3)

  override def exercise: Spec[TestEnvironment, Any] =
    suite("Methods")(
      testDefault
    )
}
