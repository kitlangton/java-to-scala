package course.archive.cohort1.m0_introduction

import course.exercises.Exercise
import zio.test.TestAspect._
import zio.test.assertTrue

//      ▄█    ▄████████  ▄█    █▄     ▄████████          ███      ▄██████▄          ▄████████  ▄████████    ▄████████  ▄█          ▄████████
//     ███   ███    ███ ███    ███   ███    ███      ▀█████████▄ ███    ███        ███    ███ ███    ███   ███    ███ ███         ███    ███
//     ███   ███    ███ ███    ███   ███    ███         ▀███▀▀██ ███    ███        ███    █▀  ███    █▀    ███    ███ ███         ███    ███
//     ███   ███    ███ ███    ███   ███    ███          ███   ▀ ███    ███        ███        ███          ███    ███ ███         ███    ███
//     ███ ▀███████████ ███    ███ ▀███████████          ███     ███    ███      ▀███████████ ███        ▀███████████ ███       ▀███████████
//     ███   ███    ███ ███    ███   ███    ███          ███     ███    ███               ███ ███    █▄    ███    ███ ███         ███    ███
//     ███   ███    ███ ███    ███   ███    ███          ███     ███    ███         ▄█    ███ ███    ███   ███    ███ ███▌    ▄   ███    ███
// █▄ ▄███   ███    █▀   ▀██████▀    ███    █▀          ▄████▀    ▀██████▀        ▄████████▀  ████████▀    ███    █▀  █████▄▄██   ███    █▀
// ▀▀▀▀▀▀                                                                                                             ▀

/** # Scala for the Java Developer
  *
  * Scala for the Java Developer is a 30-hour course that rigorously trains
  * developers having background experience in Java, Kotlin, or C# how to become
  * effective functional programmers using the Scala programming language.
  * Emphasizing the practical and pragmatic aspects of the Scala programming
  * language that help developers solve real-world problems, Scala for the Java
  * Developer is an ideal component of onboarding training for recent hires,
  * helping to solidify the knowledge and cultivate practical skills for
  * developers who will work with Scala code bases.
  *
  * Welcome to the start of an exhilarating journey into mastering Scala, one of
  * the most powerful commercial programming languages there is. It will be
  * challenging, yet it will be immensely rewarding. We want to get you up and
  * running quickly, following the Pareto principle: Which subset of Scala's
  * many features will give us the most power now, while also setting us up to
  * learn the rest?
  *
  * This repository is broken into modules (you're currently in module 0, the
  * introduction)
  *
  *   - Scala project management
  *     - Introduction to SBT
  *     - Scala project structure with SBT
  *     - Troubleshooting SBT
  *     - Primer on Metals + VS Code, IntelliJ IDEA
  *
  *   - Basic Scala syntax and semantics
  *     - Traits, classes, objects
  *     - Methods, function literals
  *     - Function types, generic types
  *     - Function composition
  *
  *   - Scala collections
  *     - Collection types
  *     - Filtering
  *     - Transforming
  *     - Combining
  *     - Problem-solving with collections
  *
  *   - Functional data modeling (ADTs)
  *     - Immutable data
  *     - Case classes
  *     - Sealed traits
  *     - Pattern Matching
  *     - Smart constructors & newtypes
  *     - Data modeling
  *
  *   - Recursion
  *     - Basic recursion
  *     - Recursive pattern matching
  *     - Recursion versus looping
  *     - Problem-solving with recursion
  *
  *   - Error handling
  *     - Try / Options / Either
  *     - Choosing the right value for the job
  *     - Exceptions versus values
  *
  *   - Introduction to functional effects, including Scala Future
  *     - Interaction with the outside world
  *     - Async & reactive programming
  *     - Concurrency & parallelism
  *     - Resiliency
  *     - Resource safety
  *     - Testing
  */
object L1_Exercises extends Exercise {

  /** ✏ EXERCISE
    *
    * Each lesson contains various exercises. Complete the following steps:
    *
    *   - Delete `@@ ignore` from the end of the test
    *   - Fix the `add` method to add its two arguments
    *   - Run the tests
    *     - In IntelliJ IDEA, you can click on the the green arrows next to the
    *       containing object
    *     - In VSCode, you can click on the run button directly above the
    *       containing object
    *     - Otherwise, you may run `testOnly *L1_Exercises*` (or the respective
    *       lesson name) after launching `sbt`
    *   - Rinse & Repeat, until all tests are passing
    */

  def add(x: Int, y: Int): Int =
    ??? // <- Fix this implementation

  val testExample =
    test("example") {
      assertTrue(add(12, 88) == 100)
    } @@ ignore // <- Delete `@@ ignore` from here, so that the test can run

  /** ✏ EXERCISE
    *
    * Another example test. Here, finish `uppercase` so it works as expected.
    */

  def uppercase(string: String): String =
    string + "???" // <- Fix this implementation

  val testExample2 =
    test("another example") {
      assertTrue(uppercase("i'm getting the hang of it") == "I'M GETTING THE HANG OF IT")
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Occasionally exercises will not have tests; there will be instruction
    * instead. This is often to explore features of the IDE, compiler, or type
    * system. For example:
    *
    * If you're using IntelliJ IDEA: Position your cursor in `fillInTheType`,
    * hit Option-Enter, and then select "Add type annotation to value
    * definition"
    *
    * If you're using VSCode: Position your cursor in `fillInTheType`, hit
    * Command-Period, and then select "Insert type annotation"
    */

  val fillInTheType = 123

  /** ☃︎ EXAMPLE
    *
    * There will also be examples. These will have some explanatory text along
    * with code, and are used to exemplify a concept or a feature of the
    * language. As always, feel free to experiment with or modify these.
    */

  val `you can use any identifier if you surround it with backticks` =
    "cool."

  // All Exercises
  def exercise = suite("Sealed Traits")(
    testExample,
    testExample2
  ) @@ sequential

}
