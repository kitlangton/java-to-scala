package course.cohort3.exercises

import course.archive.cohort2.exercises.Exercise
import zio.test.TestAspect._
import zio.test._

/** Tony Hoare famously called nulls the "billion dollar mistake".
  * Realistically, the cost they impose on production software is much higher.
  * Functional Scala does not use null values, choosing instead to reflect
  * optionality with data types that convey information at compile-time. This
  * decision results in no NullPointerExceptions, which in turn results in more
  * reliable applications with better-defined error handling.
  *
  * In this module, you will learn how to replace nulls with options.
  */
object NullBasics extends Exercise {

  /** ✏ EXERCISE
    *
    * The `parentOf` function returns `null` for some paths. Modify the function
    * to return `Option[String]` rather than `File | Null`.
    */
  val applyTest = test("apply") {
    import java.io.File

    def parentOf(file: String) = new File(file).getParent

    assertTrue(parentOf("") != null)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using the `Some` and `None` constructors of `Option` directly, construct
    * an `Option[A]` from an `A` value that might be null.
    */
  val someNoneTest = test("Some / None") {
    def fromNullable[A](a: A): Option[A] =
      ???

    val nullString = null.asInstanceOf[String]

    assertTrue(
      fromNullable(nullString) == None,
      fromNullable(0) == Some(0)
    )
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `Option#getOrElse`, use the `DefaultConfig` fallback if the
    * `loadConfig` method returns `None`.
    */
  val getOrElseTest = test("getOrElse") {
    final case class Config(host: String, port: Int)
    val DefaultConfig = Config("localhost", 12345)

    def loadConfig(): Option[Config] = None

    def config: Config = ??? // <- EDIT HERE

    assertTrue(config != null)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `Option#map`, convert an `Option[Int]` into an `Option[Char]` by
    * converting the int to a char.
    */
  val mapTest = test("map") {
    val option: Option[Int] = Some(42)

    def convert(o: Option[Int]): Option[Char] = ???

    assertTrue(
      convert(option) == Some(42.toChar),
      convert(Option.empty[Int]) == None
    )
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Implement the function `both`, which can combine two options into a single
    * option with a tuple of both results.
    */
  val bothTest = test("both") {
    def both[A, B](left: Option[A], right: Option[B]): Option[(A, B)] =
      ???

    assertTrue(
      both(Some(42), Some(24)) == Some((42, 24)),
      both(None, Some(24)) == None,
      both(None, None) == None,
      both(Some(24), None) == None
    )
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Implement the function `firstOf`, which can combine two options into a
    * single option by using the first available value.
    */
  val firstOfTest = test("firstOf") {
    def firstOf[A](left: Option[A], right: Option[A]): Option[A] =
      ???

    assertTrue(
      firstOf(None, Some(24)) == Some(24),
      firstOf(Some(24), Some(23)) == Some(24),
      firstOf(Some(24), None) == Some(24),
      firstOf(Option.empty[Int], Option.empty[Int]) == None
    )
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Implement the function `chain`, which will pass the value in an option to
    * the specified callback, which will produce another option that will be
    * returned. If there is no value in the option, then the return value of
    * `chain` will be `None`. Notice the "short-circuiting" behavior of the
    * `chain` method. What else does this remind you of?
    */
  val chainTest = test("chain") {
    def chain[A, B](first: Option[A], andThen: A => Option[B]): Option[B] =
      ???

    assertTrue(
      chain(Some(42), (x: Int) => if (x < 10) None else Some(x)) == Some(42),
      chain(Some(5), (x: Int) => if (x < 10) None else Some(x)) == None,
      chain(None, (x: Int) => if (x < 10) None else Some(x)) == None
    )
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `Option#flatMap`, simplify the following pattern-matching heavy
    * code.
    */
  val flatMapTest = test("flatMap") {
    final case class LatLong(lat: Double, long: Double)
    final case class Location(country: String, latLong: Option[LatLong])
    final case class Profile(location: Option[Location])
    final case class User(name: String, profile: Option[Profile])

    def getLatLong(user: User): Option[LatLong] =
      user.profile match {
        case None => None
        case Some(v) =>
          v.location match {
            case None    => None
            case Some(v) => v.latLong
          }
      }

    val latLong = LatLong(123, 123)

    val user = User("Holmes", Some(Profile(Some(Location("UK", Some(latLong))))))

    assertTrue(getLatLong(user) == Some(latLong))

  }

  def exercise =
    suite("basics")(
      applyTest,
      someNoneTest,
      getOrElseTest,
      mapTest,
      bothTest,
      firstOfTest,
      chainTest,
      flatMapTest
    )

}

object MigrateFromNullToOption extends Exercise {

  /** ✏ EXERCISE
    *
    * Create a null-safe version of System.property methods.
    */
  val propertyTest = test("property") {
    object SafeProperty {
      def getProperty(name: String): Option[String] = ???

      def getIntProperty(name: String): Option[Int] = ???

      def getBoolProperty(name: String): Option[Boolean] = ???
    }

    assertTrue(
      SafeProperty.getProperty("foo.bar") == None,
      SafeProperty.getIntProperty("foo.bar") == None,
      SafeProperty.getBoolProperty("foo.bar") == None,
      SafeProperty.getProperty("user.dir").isDefined
    )
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Rewrite the following code to use `Option` instead of nulls.
    */
  val sherlockTest = test("example 1") {
    final case class Address(street: String)
    final case class Profile(address: Address)
    final case class User(id: String, profile: Profile)

    val user1 =
      User("Sherlock Holmes", null)
    val user2 =
      User("Sherlock Holmes", Profile(null))
    val user3 =
      User("Sherlock Holmes", Profile(Address(null)))

    def getStreet(user: User): String =
      if (user == null) null
      else if (user.profile == null) null
      else if (user.profile.address == null) null
      else if (user.profile.address.street == null) null
      else user.profile.address.street

    def assertFails(value: => Any) = assertTrue(value == null)

    assertFails(getStreet(user1)) &&
    assertFails(getStreet(user2)) &&
    assertFails(getStreet(user3))
  } @@ ignore

  def exercise =
    suite("migrate from null to option")(
      propertyTest,
      sherlockTest
    )
}
