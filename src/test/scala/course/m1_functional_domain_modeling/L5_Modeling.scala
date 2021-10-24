package course.m1_functional_domain_modeling

import course.Lesson
import course.Lesson.???
import zio.test.TestAspect.ignore
import zio.test._

import scala.annotation.tailrec
import scala.io.StdIn

// ███╗   ███╗ ██████╗ ██████╗ ███████╗██╗     ██╗███╗   ██╗ ██████╗
// ████╗ ████║██╔═══██╗██╔══██╗██╔════╝██║     ██║████╗  ██║██╔════╝
// ██╔████╔██║██║   ██║██║  ██║█████╗  ██║     ██║██╔██╗ ██║██║  ███╗
// ██║╚██╔╝██║██║   ██║██║  ██║██╔══╝  ██║     ██║██║╚██╗██║██║   ██║
// ██║ ╚═╝ ██║╚██████╔╝██████╔╝███████╗███████╗██║██║ ╚████║╚██████╔╝
// ╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚══════╝╚══════╝╚═╝╚═╝  ╚═══╝ ╚═════╝

object L5_Modeling extends Lesson {

  /** EXERCISE
    *
    * Create a precise data model for `RelationshipStatus`, which models the relationship status of an individual:
    * married, single, divorced.
    */
  val testRelationshipStatus =
    test("RelationshipStatus") {
      type RelationshipStatus = ???

      def makeMarried: RelationshipStatus = ???

      def makeSingle: RelationshipStatus = ???

      assertTrue(makeMarried != makeSingle)
    } @@ ignore

  /** EXERCISE
    *
    * Create a precise data model for an `IceCreamOrder`, which allows a customer to choose one of four ice cream
    * flavors in either a cup, cone or milkshake (Conveyance), with an optional cherry-on-top.
    */
  val testIceCream =
    test("IceCreamOrder") {
      type IceCreamOrder = ???

      type Flavor     = ???
      type Conveyance = ???

      def rockyRoad: Flavor     = ???
      def chocolate: Flavor     = ???
      def milkshake: Conveyance = ???
      def cone: Conveyance      = ???

      def makeOrder(flavor: Flavor, conveyance: Conveyance, hasCherry: Boolean): IceCreamOrder = ???

      val order1 = makeOrder(chocolate, milkshake, true)
      val order2 = makeOrder(rockyRoad, cone, false)

      assertTrue(
        order1 == order1,
        order1 != order2
      )
    } @@ ignore

  /** EXERCISE
    *
    * Create a precise data model for a user's cryptocurrency portfolio.
    */
  val testCrypto =
    test("crypto") {
      final case class Portfolio() { // <- Complete this type
        def add(symbol: Symbol, amount: Double): Portfolio = ???
      }

      object Portfolio {
        val empty: Portfolio = ???
      }

      type Symbol = ??? // <- Complete this type

      def ETH: Symbol  = ???
      def BTC: Symbol  = ???
      def DOGE: Symbol = ???

      val p1 = Portfolio.empty.add(ETH, 1.0).add(ETH, 1.0).add(BTC, 2.0)
      val p2 = Portfolio.empty.add(BTC, 2.0).add(ETH, 2.0)
      val p3 = Portfolio.empty.add(DOGE, 9999.0)

      assertTrue(
        p1 == p2,
        p1 != p3
      )
    } @@ ignore

  /** EXERCISE
    *
    * Create a precise data model for a subscription for a SaaS product, which could be at the annual or monthly level,
    * and which could bundle different features into the plan.
    */
  val testSubscription =
    test("subscription") {
      type Features     = ???
      type Subscription = ???
      def makeFeatures(space: Int, sso: Boolean, customLogo: Boolean): Features = ???
      def makeMonthly(amount: Double, features: Features): Subscription         = ???
      def makeAnnual(amount: Double, features: Features): Subscription          = ???

      val features = makeFeatures(2048, true, true)

      def costPerMonth(subscription: Subscription): Double = ???

      assertTrue(
        makeMonthly(9.99, features) != makeAnnual(9.99, features),
        costPerMonth(makeMonthly(9.99, features)) == 9.99,
        costPerMonth(makeAnnual(120.00, features)) == 10.00
      )
    } @@ ignore

  def exercise =
    suite("Modeling")(
      testRelationshipStatus,
      testIceCream,
      testCrypto,
      testSubscription
    )
}

/** Functional Core / Imperative Shell
  *
  * The constraints of immutable data forces new program architectures. At first, this can be a challenging, if not
  * stupefying. However, these newer, more limited techniques tend to lead to better and more consistent designs.
  *
  * The most common high-level functional architecture is often referred to Functional Core / Imperative Shell. This can
  * be implemented in any language, yet it truly shines in Scala.
  *
  * With Functional Core / Imperative Shell, we try to design as much of our logic as possible with immutable data. In
  * Scala, this Functional Core would consist of ADTs formed of sealed trait and case class hierarchies. This purely
  * functional state is then wrapped in a thin layer of mutable code, the Imperative Shell, which keeps a mutable
  * reference to the immutable state and provides an interface to the messy world at the boundaries: e.g., User input,
  * web requests, database access, etc.
  *
  * Exercise
  *
  * Your task is to complete the following Counter command-line application, which is using the Functional Core /
  * Imperative Shell design.
  */
object FunctionalCounter {

  /** The Functional Core.
    *
    * This is comprised of immutable data, which contains pure methods that generate subsequent states based upon input.
    */
  final case class Model(count: Int) {

    /** Process the action and create a modified copy which represents the next iteration of the Model.
      */
    def process(action: Action): Model = ???

    /** Use the current state to render a user-readable string that will be printed to the console.
      */
    def render: String = ???
  }

  object Model {
    def empty: Model = Model(0)
  }

  sealed trait Action
  object Action {
    case object Add      extends Action
    case object Subtract extends Action
    case object Reset    extends Action

    def fromString(string: String): ProcessResult = string match {
      case "+"     => ProcessResult.succeed(Add)
      case "-"     => ProcessResult.succeed(Subtract)
      case "reset" => ProcessResult.succeed(Reset)
      case _       => ProcessResult.fail
    }
  }

  /** A monomorphic version of Option[Action]
    */
  sealed trait ProcessResult
  object ProcessResult {
    def succeed(action: Action): ProcessResult = Succeed(action)
    def fail: ProcessResult                    = Fail

    final case class Succeed(action: Action) extends ProcessResult
    final case object Fail                   extends ProcessResult
  }
  import ProcessResult._

  /** BONUS
    *
    * Rewrite the game loop into a tail-recursive function, removing any explicit mutability.
    */
  def gameLoop(): Unit = {
    var state = Model.empty
    var loop  = true
    while (loop) {
      println(state.render)
      Action.fromString(StdIn.readLine()) match {
        case Succeed(action) => state = state.process(action)
        case Fail            => loop = false
      }
    }
  }

  def main(args: Array[String]): Unit = gameLoop()

}

/** PROJECT
  *
  * Using the Functional Core / Imperative Shell paradigm, implement a game of TicTacToe. Focus first on designing the
  * immutable game state. Once the design of the core has settled, integrate it into the imperative shell—the game loop.
  */

object TicTacToe {

  /** The Functional Core, a purely functional representation of TicTacToe game state.
    *
    * Its methods, based on its input, will generate modified copies of itself—the next iteration of the game state. The
    * methods may also project out some value given the current state, e.g., whether or not the game is still active.
    */
  final case class State() {

    /** Should return true if there is not yet a winner or a draw.
      */
    def isActive: Boolean = ???

    def render: String = ???

    def nextState(move: Move): State = ???
  }

  type Move = ???
  object Move {
    def fromString(string: String): Move = ???
  }

  /** The Imperative Shell.
    *
    * This deals primarily with user input and updating a mutable reference to the game state.
    */
  def main(args: Array[String]): Unit = {
    var state = State()
    while (state.isActive) {
      println(state.render)
      val move = Move.fromString(StdIn.readLine())
      state = state.nextState(move)
    }
    println(state.render)
  }
}

/** PROJECT
  *
  * While many programming languages have a construct like case classes, few have the power of sealed traits, and most
  * do not have the pattern matching capabilities of Scala. With the combination of these powerful features, you can
  * construct very precise data models that eliminate runtime errors and make it easier than ever to test and maintain
  * code.
  *
  * In this graduation project, you will gain experience constructing precise data models using case classes and sealed
  * traits.
  */
object DataGraduation {

  sealed trait Command
  object Command {
    case object Exit                        extends Command
    final case class Look(what: String)     extends Command
    final case class Go(where: String)      extends Command
    final case class Take(what: String)     extends Command
    final case class Drop(what: String)     extends Command
    final case class Fight(who: String)     extends Command
    final case class TalkTo(who: String)    extends Command
    final case class Unknown(input: String) extends Command

    def fromString(input: String): Command =
      input.trim.toLowerCase.split("\\w+").toList match {
        case "exit" :: Nil                => Exit
        case "look" :: what :: Nil        => Look(what)
        case "go" :: where :: Nil         => Go(where)
        case "take" :: what :: Nil        => Take(what)
        case "drop" :: what :: Nil        => Drop(what)
        case "fight" :: who :: Nil        => Fight(who)
        case "talk" :: "to" :: who :: Nil => TalkTo(who)
        case _                            => Unknown(input)
      }
  }

  /** EXERCISE
    *
    * Construct a data model for the state of a game world in a text-based role-playing game. The data model should
    * represent the player character, the map of the game world, items and characters in the game world, and anything
    * else relevant to the game.
    */
  final case class State(playerName: String)
  final case class Step(nextState: Option[State], output: String)

  /** EXERCISE
    *
    * Implement the `nextStep` function in such a fashion that new states for the game world are constructed from both
    * the old state and the current command read from the user.
    */
  def nextStep(state: State, command: Command): Step = ???

  @tailrec
  def gameLoop(state: State): Unit = {
    println("look | go | tak | drop | fight | talk to")
    print("> ")
    val line                    = StdIn.readLine()
    val command                 = Command.fromString(line)
    val Step(nextState, output) = nextStep(state, command)

    println(output)

    nextState match {
      case Some(state) => gameLoop(state)
      case None        => ()
    }
  }

  def main(args: Array[String]): Unit = {
    println("Welcome to the game! What is your name?")
    val name  = StdIn.readLine()
    val state = State(name)
    gameLoop(state)
  }
}
