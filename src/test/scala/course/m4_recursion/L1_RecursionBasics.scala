package course.m4_recursion
import course.Lesson
import zio.ZIO
import zio.console.Console
import zio.test._
import zio.test.TestAspect._

import scala.annotation.tailrec

/** One of the backbones of applications is the ability to loop: looping is used
  * both for most algorithms (such as sorting lists), as well as for large-scale
  * application behavior like repeatedly reading results from a database query
  * or repeatedly handling requests from a socket. Functional applications don't
  * use traditional loops, but instead, they use recursion, which has the same
  * power as looping but in a package that lends itself to immutable data and
  * pure functions.
  *
  * In this module, you will explore using recursion to solve some of the same
  * tasks you would use looping to solve in a procedural programming language.
  */
object L1_RecursionBasics extends Lesson {

  /** ✏ EXERCISE
    *
    * Using recursion, compute the sum of a list of integers.
    */
  val sumTest = test("sum") {
    def sum(list: List[Int]): Int = ???

    assertTrue(sum(List(1, 2, 3, 4, 5)) == 15)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using recursion, compute the maximum of a list of integers.
    */
  val maxTest = test("max") {
    def max(list: List[Int]): Int = ???

    assertTrue(max(List(1, 7, 3, 2, 4, 5)) == 7)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using recursion, determine if a number is prime.
    */
  val primeTest = test("prime") {
    def isPrime(n: Int): Boolean = {
      def loop(n: Int, divisor: Int): Boolean = ???

      loop(n, 2)
    }

    assertTrue(!isPrime(4) && isPrime(7) && isPrime(11))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using recursion, compute the nth fibonacci number. The fibonacci sequence
    * is given by, 0, 1, 1, <sum of two previous nums>...
    */
  val fibsTest = test("fibs") {
    def fib(n: Int): Int = ???

    assertTrue(fib(3) == 2 && fib(4) == 3 && fib(5) == 5)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using recursion, sort the provided list, by taking the head, and sorting
    * those less than the head, and those not less than the head (separately),
    * then concatenating them in the right order.
    */
  val pivotTest = test("pivot sort") {
    def sort[A](list: List[A])(implicit ordering: Ordering[A]): List[A] = ???

    assertTrue(sort(List(9, 23, 1, 5)) == List(1, 5, 9, 23))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using recursion, implement a method to loop until a predicate is
    * satisfied.
    */
  val loopTest = test("loop") {
    def loop[S](start: S)(pred: S => Boolean)(iterate: S => S): S = ???

    val inc = loop(0)(_ < 10)(_ + 1)

    assertTrue(inc == 10)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using recursion, implement a method that repeats the specified action
    * again and again, until the predicate is true.
    */
  val repeatTest = test("repeat") {
    var input = "John" :: "Scotty" :: "Bob" :: "Sherlock" :: Nil

    val readLine: () => String = () =>
      input match {
        case Nil => ""
        case head :: tail =>
          input = tail
          head
      }

    def repeatWhile[A, S](action: () => A)(pred: A => Boolean)(reducer: (A, A) => A): A = ???

    val result = repeatWhile(readLine)(_ == "Sherlock")((a, b) => b)

    assertTrue(result == "Sherlock")
  } @@ ignore

  def exercise =
    suite("Recursion Basics")(
      sumTest,
      maxTest,
      primeTest,
      fibsTest,
      pivotTest,
      loopTest,
      repeatTest
    )
}

object L2_TailRecursion extends Lesson {

  /** ✏ EXERCISE
    *
    * Write a tail-recursive version of the previous `sum`.
    */
  val sumTest = test("sum") {
    // @tailrec
    def sum(list: List[Int]): Int = ???

    assertTrue(sum(List(1, 2, 3, 4, 5)) == 15)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Write a tail-recursive version of the previous `max`.
    */
  val maxTest = test("max") {
    // @tailrec
    def max(list: List[Int]): Int = ???

    assertTrue(max(List(1, 7, 3, 2, 4, 5)) == 7)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Write a tail-recursive version of the previous `loop`.
    */
  val loopTest = test("loop") {
    // @tailrec
    def loop[S](start: S)(pred: S => Boolean)(iterate: S => S): S = ???

    val inc = loop(0)(_ < 10)(_ + 1)

    assertTrue(inc == 10)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Write a tail-recursive version of the previous `repeat`.
    */
  val repeatTest = test("repeat") {
    var input = "John" :: "Scotty" :: "Bob" :: "Sherlock" :: Nil

    val readLine: () => String = () =>
      input match {
        case Nil => ""
        case head :: tail =>
          input = tail
          head
      }

    // @tailrec
    def repeatWhile[A, S](action: () => A)(pred: A => Boolean)(reducer: (A, A) => A): A = ???

    val result = repeatWhile(readLine)(_ == "Sherlock")((a, b) => b)

    assertTrue(result == "Sherlock")
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Try to find a way to write the fib sequence using tail recursion.
    *
    * WARNING: Advanced.
    */
  val fibsTest = test("fibs") {
    // @tailrec
    def fib(n: Int): Int = ???

    assertTrue(fib(3) == 2 && fib(4) == 3 && fib(5) == 5)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Try to find a way to write the pivot sort using tail recursion.
    *
    * WARNING: Advanced.
    */
  val pivotSortTest = test("pivot sort") {
    // @tailrec
    def sort[A](list: List[A])(implicit ordering: Ordering[A]): List[A] = ???

    assertTrue(sort(List(9, 23, 1, 5)) == List(1, 5, 9, 23))
  } @@ ignore

  def exercise =
    suite("Tail Recursion")(
      sumTest,
      maxTest,
      loopTest,
      repeatTest,
      fibsTest,
      pivotSortTest
    )
}

/** Recursion is the general-purpose replacement for looping in functional
  * Scala. While recursion should be avoided when simpler alternatives exist
  * (e.g. foldLeft on List), when not possible, recursion provides a very
  * powerful tool that can solve the most complex iterative problems.
  *
  * In this graduation project, you get to take a break and experiment with
  * recursion in a functional effect system as you implement a game of hangman.
  */
object RecursionGraduation {

  /** ✏ EXERCISE
    *
    * Implement an effect that gets a single, lower-case character from the
    * user.
    */
  def getChoice: Char = ???

  /** ✏ EXERCISE
    *
    * Implement an effect that prompts the user for their name, and returns it.
    */
  def getName: String = ???

  /** ✏ EXERCISE
    *
    * Implement an effect that chooses a random word from the dictionary. The
    * dictionary is `Dictionary`.
    */
  def chooseWord: String = ???

  /** ✏ EXERCISE
    *
    * Implement the main game loop, which gets choices from the user until the
    * game is won or lost.
    */
  def gameLoop(oldState: State): Unit = ???

  def renderState(state: State): Unit = {

    /** f n c t o
      *   - - - - - - -
      *
      * Guesses: a, z, y, x
      */
    val word =
      state.word.toList
        .map(c => if (state.guesses.contains(c)) s" $c " else "   ")
        .mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = " Guesses: " + state.guesses.mkString(", ")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    println(text)
  }

  final case class State(name: String, guesses: Set[Char], word: String) {
    def failures: Int = (guesses -- word.toSet).size

    def playerLost: Boolean = failures > 10

    def playerWon: Boolean = (word.toSet -- guesses).isEmpty

    def addChar(char: Char): State = copy(guesses = guesses + char)
  }

  final case class Step(output: String, keepPlaying: Boolean, state: State)

  def analyzeChoice(
      oldState: State,
      char: Char
  ): Step = {
    val newState = oldState.addChar(char)

    if (oldState.guesses.contains(char))
      Step("You already guessed this character!", true, newState)
    else if (newState.playerWon)
      Step("Congratulations, you won!!!", false, newState)
    else if (newState.playerLost)
      Step(s"Sorry, ${oldState.name}, you lost. Try again soon!", false, newState)
    else if (oldState.word.contains(char))
      Step(s"Good work, ${oldState.name}, you got that right! Keep going!!!", true, newState)
    else Step(s"Uh, oh! That choice is not correct. Keep trying!", true, newState)
  }

  /** ✏ EXERCISE
    *
    * Execute the main function and verify your program works as intended.
    */
  def main(args: Array[String]): Unit = {
    val name  = getName
    val word  = chooseWord
    val state = State(name, Set(), word)
    renderState(state)
    gameLoop(state)
  }

}
