package course.cohort3.exercises

object E1_Methods extends App {
  // Default Parameters
  // ==================

  /** ✏ EXERCISE
    *
    * Give the `default` parameter a default value of "Anonymous".
    */
  def userNameOrDefault(name: String, default: String = "Anonymous"): String =
    if (name.isEmpty) default
    else name

  // 💬 When you're ready, uncomment the following line:
  assert(userNameOrDefault("") == "Anonymous")

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
  outOfOrderSomeFood(isDelicious = true, price = 555.55, name = "golden pizza")

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
  greet("Kit")

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
  greetThrice(candyman)
  greetThrice(throw new IndexOutOfBoundsException())

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
}
