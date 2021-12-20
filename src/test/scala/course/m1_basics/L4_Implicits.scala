package course.m1_basics

import course._
import course.Lesson.???
import course.m1_basics.ImplicitConversions.StringUtils.yell
import zio.test.TestAspect.ignore
import zio.test.assertTrue

import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.language.implicitConversions
//import scala.language.implicitConversions

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

  // An explicit parameter list
  def explicitMethod(int: Int) =
    s"Here's the Int you gave me: $int"

  // An implicit parameter list
  def implicitMethod(implicit int: Int) =
    s"Here's the Int I found: $int"

  // Both of these methods can be called in the usual way
//  explicitMethod(100) // => Here's the Int you gave me: 100
//  implicitMethod(100) // => Here's the Int I found: 100

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

  def validateConfig(config: Config): Boolean =
    config.port > 1000

  def createConnection(name: String, config: Config): Unit =
    validateConfig(config)

  def setUpDatabase(name: String, timeout: Int, config: Config): Unit = {
    validateConfig(config)
    createConnection(name, config)
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

    lazy val summoned: Butler = ??? // <-- Summon jeeves by using the `implicitly` method

    assertTrue(summoned == Butler("Jeeves"))
  } @@ ignore

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

    lazy val summoned: Butler = tacitly[Butler]

    assertTrue(summoned == Butler("Humphrey"))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * One thing to keep in mind is that when the Scala compiler searches for an
    * implicit (a process known as "Implicit Resolution"), there must not be
    * multiple implicits defined for the same type in scope.
    *
    * Try uncommenting the second implicit below, and see what happens.
    */
  final case class Spartacus()

  implicit val iAmSpartacus: Spartacus = Spartacus()
//  implicit val noIAmSpartacus: Spartacus = Spartacus()

  val spartacus = implicitly[Spartacus]

  /** If you compile, you'd see the following `ambiguous implicit values` error:
    *
    * {{{
    *   ambiguous implicit values:
    * both value noIAmSpartacus in object ImplicitParameters of type course.m1_basics.ImplicitParameters.Spartacus
    * and value iAmSpartacus in object ImplicitParameters of type course.m1_basics.ImplicitParameters.Spartacus
    * match expected type course.m1_basics.ImplicitParameters.Spartacus
    * }}}
    */

  /** ✏ EXERCISE
    *
    * It's also possible to define implicit methods. These can require their own
    * implicit parameter lists, potentially firing off other implicit searches.
    * How exciting!
    *
    * By only changing the value of `implicitDouble`, see if you can get the
    * test to pass.
    */

  val implicitMethodTest = test("implicit methods") {
    implicit def implicitBoolean(implicit double: Double): Boolean =
      double > 99.99

    implicit val disturbance: Double = 15.5 // <- Only change this value

    def sense(implicit boolean: Boolean): String =
      if (boolean) "I sense a disturbance" else "Who wants to go get ice cream?"

    assertTrue(sense == "I sense a disturbance")
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Complete the unfinished method below to test your knowledge of implicit
    * definitions.
    */

  val customImplicitMethodTest = test("custom implicit method") {
    final case class Name(string: String)
    final case class Age(int: Int)
    final case class Person(name: Name, age: Age)

    implicit val name: Name = Name("Olive")
    implicit val age: Age   = Age(32)

    implicit def implicitPerson: Person = ??? // <-- Finish this method

    assertTrue(implicitly[Person] == Person(Name("Olive"), Age(32)))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Because implicit errors can be a little confusing, Scala provides the
    * `implicitNotFound` annotation, which lets you override the default
    * implicit not found error.
    *
    * uncomment the labeled line and then compile to see the custom error.
    */
  final case class LightSwitch()

  def feelAroundInTheDark(implicit
      @implicitNotFound(
        "Uh oh. Oh no! Where's the light switch? I can't find the light switch!"
      ) lightSwitch: LightSwitch
  ): String =
    "I FOUND IT!"

  // feelAroundInTheDark // <- Uncomment this line to see the error

  /** ✏ EXERCISE
    *
    * Instead of placing the `implicitNotFound` annotation on an implicit
    * parameter, you can also annotate the type itself, and the compiler will
    * use this error whenever it cannot find that type as in implicit.
    *
    * It is also possible to interpolate types into the error message. The
    * syntax looks just like string interpolation, although note that there is
    * no leading 's'.
    */

  @implicitNotFound("Has anyone seen my box of ${A}")
  final case class Box[A](contents: List[A])

  def getTheBox[A](implicit box: Box[A]): List[A] = box.contents

  // getTheBox[Doughnuts] <-- Uncomment this line to see the error

  def exercise = suite("Implicits")(
    implicitlyTest,
    tacitlyTest,
    implicitMethodTest,
    customImplicitMethodTest
  )
}

/** ☃︎ EXAMPLE
  *
  * Implicit resolution is a process by which the Scala compiler searches for an
  * implicit value of a particular type. Though it may sometimes seems
  * mysterious, is does follow a predefined series of steps. Understanding this
  * can make implicits much less magical.
  */

/** The MacGuffin is an item of great and ineffable value. It will be the target
  * of the next few implicit searches.
  */
final case class MacGuffin(value: Int)

object ImplicitResolution_InScope {

  /** This MacGuffin is defined in the scope, and thus trivially available to
    * the implicit search below.
    */
  implicit val macGuffin: MacGuffin = MacGuffin(100)

  implicitly[MacGuffin]
}

object treasure {
  implicit val macGuffin: MacGuffin = MacGuffin(200)
}

object ImplicitResolution_ImportedExplicitly {

  /** The implicit `macGuffin` is imported, and will thus be found.
    */
  import treasure.macGuffin

  implicitly[MacGuffin]
}

object riches {
  implicit val macGuffin: MacGuffin = MacGuffin(100_000)
}

object ImplicitResolution_ImportedWildcard {

  /** A wildcard import will also bring in any implicit values defined within.
    */
  import riches._

  implicitly[MacGuffin]
}

/** ✏ EXERCISE
  *
  * Try commenting out each implicit import/definition before running the main
  * method in order to realize that the precedence moves from definition, to
  * explicit import, to wildcard import.
  */
object ImplicitResolution_Precedence {
  import riches._           // 3
  import treasure.macGuffin // 2

  def main(args: Array[String]): Unit = {
    implicit val macGuffin: MacGuffin = MacGuffin(0) // 1
    println(implicitly[MacGuffin])
  }
}

/** ✏ EXERCISE
  *
  * After explicit imports, the compiler will search for implicit values in the
  * companion object of the type.
  *
  * Try renaming the companion object and notice how the implicit value is no
  * longer found.
  */
object ImplicitResolution_Companion {
  final case class TantalizingCube(material: String)

  object TantalizingCube {
    implicit val tungsten: TantalizingCube =
      TantalizingCube("Tungsten")
  }

  implicitly[TantalizingCube]

}

/** If the implicit value being search for is of a parameterized type, then the
  * compiler will also search for implicits inside of the companion objects of
  * the type parameters.
  */
object ImplicitResolution_CompanionOfArguments {
  trait Nickname[A] {
    def nickname: String
  }

  final case class Friend(name: String) {
    override def toString: String = name
  }

  object Friend {
    implicit val nickname: Nickname[Friend] =
      new Nickname[Friend] {
        override def nickname: String = "Buddy"
      }
  }

  final case class Money(amount: Int)

  def printNickname[A](value: A)(implicit nickname: Nickname[A]): Unit =
    println(s"${nickname.nickname}: $value")

  def main(args: Array[String]): Unit =
    printNickname(Friend("Kyle"))
//    printNickname(Money(55_555)) // <- Uncomment this line
}

/** ☃︎ EXAMPLE
  *
  * Once you start using parameterized types as implicit parameters, things
  * start to get very interesting.
  *
  * One use case is to create "type tags". Type tags can used to circumvent type
  * erasure.
  */
object TypeErasure {

  def label[A](list: List[A]): String =
    list match {
      case strings: List[String]   => "List of Strings"
      case ints: List[Int]         => "List of Int"
      case booleans: List[Boolean] => "List of Boolean"
      case _                       => "Other List"
    }

  def main(args: Array[String]): Unit =
    println(label(List("a", "b", "c")))
  //    println(label(List(1, 2, 3)))
  //    println(label(List(true, false, true)))

}

/** ☃︎ EXAMPLE
  *
  * Type tags to the rescue.
  */
object TypeTags {
  sealed trait TypeTag[A]

  implicit case object IntTag     extends TypeTag[Int]
  implicit case object StringTag  extends TypeTag[String]
  implicit case object BooleanTag extends TypeTag[Boolean]

  def label[A](list: List[A])(implicit tag: TypeTag[A]): String =
    tag match {
      case IntTag     => "List of Int"
      case StringTag  => "List of String"
      case BooleanTag => "List of Boolean"
    }

  def main(args: Array[String]): Unit = {
    println(label(List("a", "b", "c")))
    println(label(List(1, 2, 3)))
    println(label(List(true, false, true)))
  }
}

/** ☃︎ EXAMPLE
  *
  * Implicits also underlie the "type class" pattern. Type classes allow you to
  * essentially create interfaces without subtyping. They're more of a pattern
  * that involves implicits, rather than a separate language feature.
  */
object TypeClasses1 {

  trait JsonEncodable {
    def encode: String
  }

  final case class File(name: String, size: Int) extends JsonEncodable {
    override def encode: String = s"""{"name": "$name", "size": $size}"""
  }

  final case class Directory(name: String, contents: List[File]) extends JsonEncodable {
    override def encode: String =
      s"""{"name": "$name", "contents": [${contents.map(_.encode).mkString(", ")}]}"""
  }

  def encode(jsonEncodeable: JsonEncodable): String =
    jsonEncodeable.encode

  def encodeList(list: List[JsonEncodable]): String =
    list.map(_.encode).mkString("[", ", ", "]")

  val exampleDirectory =
    List(
      Directory(
        "movies",
        List(
          File("The Matrix", 187853),
          File("Hackers", 75881),
          File("The Room", 65854)
        )
      ),
      Directory(
        "stuff",
        List(
          File("passwords.txt", 1273),
          File("todo_list.png", 5812),
          File("taxes_2021.ogg", 1234)
        )
      )
    )

  def main(args: Array[String]): Unit =
    println(encodeList(exampleDirectory))
}

object TypeClasses2 {
  trait Decodable {
    def decode: ???
  }

  final case class Age(int: Int) extends Decodable {
    override def decode: ??? = ???
  }

//  def decode(string: String): ???
}

/** Now let's explore the second type of implicit: Implicit Conversions
  */
object ImplicitConversions {

  final case class Name(value: String) {
    def announce(): Unit = println(s"My name is $value! And I am confused!")
  }

  /** This is an implicit conversion.
    *
    * This will convert `String` to `Name`
    */
  implicit def stringToName(string: String): Name = Name(string)

  /** Implicit conversions will activate for whenever you're attempting to use
    * some type `B` in the place of another `A` AND there is an implicit method
    * defined from `A` to `B`.
    *
    * In this case, we're using a String where a Name is expected, and since we
    * have a method `String => Name` defined above, it will be called
    * implicitly.
    */
  val name: Name = "Billy Joe"

  // 1. Define a method that expects a Name, then use a String
  // 2. Use a method on some type `A` that is actually defined on `B`
}

/** This brings us to the pattern of adding extension methods.
  */
object ExtensionMethods {}

object ImplicitClasses {}
