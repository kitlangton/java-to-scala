package course.exercises

import zio.test.TestAspect.ignore
import zio.test.assertTrue

// ███████╗███████╗ █████╗ ██╗     ███████╗██████╗     ████████╗██████╗  █████╗ ██╗████████╗███████╗
// ██╔════╝██╔════╝██╔══██╗██║     ██╔════╝██╔══██╗    ╚══██╔══╝██╔══██╗██╔══██╗██║╚══██╔══╝██╔════╝
// ███████╗█████╗  ███████║██║     █████╗  ██║  ██║       ██║   ██████╔╝███████║██║   ██║   ███████╗
// ╚════██║██╔══╝  ██╔══██║██║     ██╔══╝  ██║  ██║       ██║   ██╔══██╗██╔══██║██║   ██║   ╚════██║
// ███████║███████╗██║  ██║███████╗███████╗██████╔╝       ██║   ██║  ██║██║  ██║██║   ██║   ███████║
// ╚══════╝╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝╚═════╝        ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝   ╚═╝   ╚══════╝

/** In functional Scala, nearly all data is immutable. Data models are
  * constructed entirely from either "records", which have multiple fields of
  * different types, or "enumerations", which have multiple cases with different
  * structure. Scala provides powerful functionality built into records and
  * enumerations. Everything about the way you construct these data models to
  * the way you use them is different than in an object-oriented programming
  * language. The emphasis is on making very precise data models that cannot be
  * used to store "bad data", which results in eliminating runtime errors and
  * keeping bad data out of databases and third-party systems.
  *
  * In this module, you will learn how to adopt the functional tools that Scala
  * gives you to solve data modeling problems in a precise way that improves the
  * maintainability and reliability of your software.
  */
object E8_SealedTraits extends Exercise {

  /** ✏ EXERCISE
    *
    * Add Pluto (back!) to the Planet sealed trait, and then recompile the
    * project.
    *
    * Notice the most delightful error!
    */

  sealed trait Planet

  object Planet {
    case object Mercury extends Planet
    case object Venus   extends Planet
    case object Earth   extends Planet
    case object Mars    extends Planet
    case object Jupiter extends Planet
    case object Saturn  extends Planet
    case object Uranus  extends Planet
    case object Neptune extends Planet

    val allPlanets: List[Planet] =
      List(Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune)
  }

  final case class SurfaceGravity(g: Double)

  import Planet._

  /** Returns the surface gravity, as a percentage of Earth's gravity, for the
    * given planet.
    */
  def gravity(planet: Planet): SurfaceGravity = planet match {
    case Mercury => SurfaceGravity(0.377)
    case Venus   => SurfaceGravity(0.905)
    case Earth   => SurfaceGravity(1.0)
    case Mars    => SurfaceGravity(0.379)
    case Jupiter => SurfaceGravity(2.528)
    case Saturn  => SurfaceGravity(1.065)
    case Uranus  => SurfaceGravity(0.886)
    case Neptune => SurfaceGravity(1.137)
  }

  /** Returns the official bird for the given planet.
    */
  def officialBird(planet: Planet): String = planet match {
    case Mercury => "No birds!"
    case Venus   => "No birds!"
    case Earth   => "Parrot"
    case Mars    => "I'm afraid there are no birds here."
    case Jupiter => "None"
    case Saturn  => "Bird-less"
    case Uranus  => "Completely bird-less"
    case Neptune => "Ix-nay on the irds-bay"
  }

  val testPlanet =
    test("Planet") {
      val allGravities = Planet.allPlanets.map(gravity)
      val allBirds     = Planet.allPlanets.map(officialBird)
      assertTrue(
        allGravities.length >= 8,
        !allBirds.contains("What?")
      )
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Now do the same thing with the Stringly-Typed Planet
    *
    * Note the difference.
    */

  object StringPlanet {
    val Mercury = "Mercury"
    val Venus   = "Venus"
    val Earth   = "Earth"
    val Mars    = "Mars"
    val Jupiter = "Jupiter"
    val Saturn  = "Saturn"
    val Uranus  = "Uranus"
    val Neptune = "Neptune"

    val allPlanets: List[String] = List(Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune)
  }

  /** Returns the surface gravity, as a percentage of Earth's gravity, for the
    * given planet.
    */
  def gravity(planet: String): SurfaceGravity =
    if (planet == "Mercury") SurfaceGravity(0.377)
    else if (planet == "Venus") SurfaceGravity(0.905)
    else if (planet == "Earth") SurfaceGravity(1.0)
    else if (planet == "Mars") SurfaceGravity(0.379)
    else if (planet == "Jupiter") SurfaceGravity(2.528)
    else if (planet == "Saturn") SurfaceGravity(1.065)
    else if (planet == "Uranus") SurfaceGravity(0.886)
    else if (planet == "Neptune") SurfaceGravity(1.137)
    else throw new Error(s"NEVER HEARD OF $planet?")

  /** Returns the official bird for the given planet.
    */
  def officialBird(planet: String): String =
    if (planet == "Mercury") "No birds!"
    else if (planet == "Venus") "No birds!"
    else if (planet == "Earth") "Parrot"
    else if (planet == "Mars") "I'm afraid there are no birds here."
    else if (planet == "Jupiter") "None"
    else if (planet == "Saturn") "Bird-less"
    else if (planet == "Uranus") "Completely bird-less"
    else if (planet == "Neptune") "Ix-nay on the irds-bay"
    else "What?"

  val testStringPlanet =
    test("String Planet") {
      val allGravities = StringPlanet.allPlanets.map(gravity)
      val allBirds     = StringPlanet.allPlanets.map(officialBird)
      assertTrue(
        allGravities.length >= 8,
        !allBirds.contains("What?")
      )
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Use "match (exhaustive)" to generate the pattern match, then finish
    * implementing `isRed`.
    */
  sealed trait Color

  object Color {
    case object Red   extends Color
    case object Green extends Color
    case object Blue  extends Color
  }

  def isRed(color: Color): Boolean = ??? // <- write "color match" then select "match (exhaustive)"

  val testMatchExhaustive =
    test("match exhaustive") {
      import Color._

      assertTrue(
        isRed(Red),
        !isRed(Blue),
        !isRed(Green)
      )
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Add the following cases to this `Direction` sealed trait:
    *   - Up, Down, Left, Right
    *
    * Then, implement the methods on the trait using exhaustive pattern
    * matching.
    */
  sealed trait Direction {
    def turnRight: Direction = ???

    def turnLeft: Direction = ???

    def flip: Direction = ???
  }

  object Direction {
    // ADD THE CASES HERE
  }

  val testDirection =
    test("Direction") {

      lazy val anyDirection: Direction = ??? // <- Instantiate a Direction here

      // Uncomment these once they've been implemented
//      lazy val down: Direction  = Down
//      lazy val up: Direction    = Up
//      lazy val left: Direction  = Left
//      lazy val right: Direction = Right

      assertTrue(
        anyDirection.turnRight.turnRight.turnRight.turnRight == anyDirection,
        anyDirection.turnRight.turnLeft == anyDirection,
        anyDirection.flip.flip == anyDirection
//        up.flip == down,
//        right.flip == left,
//        right.turnRight == down,
      )
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Complete the following User sealed trait that contains the following
    * cases:
    *   - RegisteredUser that has a UUID id and a String email address
    *   - GuestUser which has no parameters
    *
    * Then complete the missing implementations of `identifier`, `guest`, and
    * `user`.
    */

  trait User {

    /** This should return the User's email address if they're registered, or
      * "Guest" if they're a guest.
      */
    def identifier: String = ???

  }

  object User {
    // 1. CREATE THE CASES

    // 2. COMPLETE THE FOLLOWING CONSTRUCTORS

    /** Creates a Guest user.
      */
    val guest: User = ???

    /** Creates a registered user with the given email address and a random
      * UUID.
      */
    def apply(email: String): User = ???
  }

  val testUser = test("User") {
    val registered = User("martin@scala.com")
    val guest      = User.guest

    assertTrue(
      registered.identifier == "martin@scala.com",
      guest.identifier == "Guest"
    )
  } @@ ignore

  // All Exercises
  def exercise = suite("Sealed Traits")(
    testPlanet,
    testStringPlanet,
    testMatchExhaustive,
    testDirection,
    testUser
  )

}
