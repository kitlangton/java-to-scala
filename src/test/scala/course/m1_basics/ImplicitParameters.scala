package course.m1_basics

import course.Lesson
import zio.test.assertTrue

/** Implicits are very unique and powerful feature of Scala. They also can be
  * incredibly confusing to newcomers: Secretly changing the behavior of code
  * from behind the scenes; at their worst, this can make Scala feel like a hall
  * of mirrors.
  *
  * Luckily, there are some simple rules and repeatable patterns that we can
  * address systematically. Furthermore, there are some tools built into
  * IntelliJ that can help.
  *
  * At its core, there are just two types of implicits:
  *   1. Implicit parameters
  *   1. Implicit conversions
  *
  * Let's begin with implicit parameters:
  */
object ImplicitParameters extends Lesson {

  // An explicit parameter
  def explicitMethod(int: Int) =
    s"Here's the Int you gave me: $int"

  // An implicit parameter
  def implicitMethod(implicit int: Int) =
    s"Here's the Int I found: $int"

  // Both of these methods can be called in the usual way
  explicitMethod(100)
  implicitMethod(100)

  /** ✏ EXERCISE
    *
    * But now let's call both methods without providing an argument. Uncomment
    * the following lines and note the errors:
    */
  // val missingExplicitArguments = explicitMethod // <- Uncomment this line
  // val missingImplicitArguments = implicitMethod // <- Uncomment this line

  /** The first method call complains that its missing its arguments, which
    * makes perfect sense.
    *
    * The second, however, says: "No implicits found for parameter int: Int."
    * Let's see how we can address that second error.
    *
    * Comment out both of the lines again so we can continue without errors.
    */

  /** ✏ EXERCISE
    *
    * Uncomment the following and see what happens. (It should compile this
    * time!)
    */
  // implicit val fortyTwo: Int   = 42             // <- Uncomment this line
  // val findingImplicitArguments = implicitMethod // <- Uncomment this line

  /** Now that we have an implicit defined in scope, we can call the method
    * without providing an argument, and the compiler will find and supply the
    * implicit value for us.
    *
    * Placing the implicit keyword at the start of a parameter list declares the
    * parameters in that list as implicit, and will search for implicit values
    * of the corresponding types when the method is called without providing
    * those values explicitly.
    */

  /** ✏ EXERCISE
    *
    * Uncomment the labeled method call below, and then get it to compile by
    * creating an implicit value.
    */

  def needsImplicitString(implicit string: String) =
    s"You dropped this: $string"

//  needsImplicitString // <- Uncomment this line

  /** ✏ EXERCISE
    *
    * The implicit keyword must be used at the start of a parameter list, and
    * makes EVERY parameter in that list implicit.
    *
    * Here, in `replicate`, both `message` and `times` will be provided
    * implicitly as long as there are implicit values of the corresponding types
    * in scope.
    *
    * If you have followed the exercises in order, you should now have both an
    * implicit Int and String defined above. So try calling `replicate` without
    * arguments and observe that it compiles successfully.
    */
  def replicate(implicit message: String, times: Int): String = message * times

  /** ✏ EXERCISE
    *
    * There can only be one implicit parameter list per method, and it must be
    * the final parameter list.
    *
    * Try commenting out the second method below and note that it doesn't
    * compile.
    */

  def finalParameterList(w: Int, x: String)(implicit y: Int, z: String) =
    s"$w, $x, $y, $z"

  // def firstParameterList(implicit w: Int, x: String)(y: Int, z: String) =
  //   s"$w, $x, $y, $z"

  /** ✏ EXERCISE
    *
    * One real world usage of implicits you'll encounter is to reduce the
    * boilerplate of argument passing, usually when something dependency must be
    * threaded, unchanged, through many nested functions.
    *
    * Refactor the following code to thread the `Config` parameter through the
    * nested method calls implicitly.
    *
    * NOTE: This isn't necessarily a good use of implicits, and can make code
    * confusing and difficult to refactor, but it is something you'll see in the
    * wild.
    */

  final case class Config(port: Int, host: String)

  def validateConfig(config: Config): Unit =
    println(s"Validating $config")

  def createConnection(name: String, config: Config): Unit = {
    validateConfig(config)
    println(s"Connecting to $config for my database named $name")
  }

  def setUpDatabase(name: String, timeout: Int, config: Config): Unit = {
    println(s"Hopefully I can do this before $timeout milliseconds!")
    validateConfig(config)
    createConnection(name, config)
    println(s"Setting up database named $name with config $config")
  }

  val config: Config = Config(8080, "localhost")

  setUpDatabase("Cool Database", 10_000, config)

  /** ✏ EXERCISE
    *
    * Scala's built-in `implicitly` method will simply search for an implicit
    * value of the given type, returning that value.
    */

  val implicitlyTest = test("implicitly") {
    final case class Butler(name: String)

    implicit val jeeves: Butler = Butler("Jeeves")

    val summoned: Butler = ??? // <-- Summon jeeves by using the `implicitly` method

    assertTrue(summoned == Butler("Jeeves"))
  }

  /** ✏ EXERCISE
    *
    * The definition of `implicitly` is actually dead simple. Let's make our
    * own.
    *
    * Without using `implicitly`, define your a method called `tacitly` that
    * will return an implicit value of the provided type.
    */

  val tacitlyTest = test("tacitly") {
    final case class Butler(name: String)

    implicit val jeeves: Butler = Butler("Humphrey")

    def tacitly[A] = ??? // <-- Finish this method

    val summoned: Butler = tacitly[Butler]

    assertTrue(summoned == Butler("Humphrey"))
  }

  /** ✏ EXERCISE
    *
    * One thing to keep in mind is that in order for Scala to find an implicit
    * (a process called "Implicit Resolution"), there must not be multiple
    * implicits defined for the same type in scope.
    *
    * Try uncommenting the second implicit below, and see what happens.
    */

  final case class Spartacus()

  implicit val iAmSpartacus: Spartacus = Spartacus()
//  implicit val noIAmSpartacus: Spartacus = Spartacus()

  val spartacus = implicitly[Spartacus]

  /** It's also possible to define implicit methods. These can require their own
    * implicit parameter lists.
    */

  implicit def implicitBoolean(implicit double: Double): Boolean =
    double > 99.99

  def getImplicitBoolean(implicit boolean: Boolean) =
    s"The implicit Boolean is: $boolean"

  implicit val implicitDouble: Double = 55.0

  getImplicitBoolean

  /** ✏ EXERCISE
    *
    * Complete the unfinished method below.
    */

  val implicitMethodsTest = test("implicitMethods") {
    final case class Name(string: String)
    final case class Age(int: Int)
    final case class Person(name: Name, age: Age)

    implicit val name: Name = Name("Olive")
    implicit val age: Age   = Age(30)

    implicit def implicitPerson: Person = ??? // <-- Finish this method

    assertTrue(implicitPerson == Person(Name("Olive"), Age(30)))
  }

  def exercise = suite("Implicits")(
    tacitlyTest,
    implicitlyTest
  )
}
