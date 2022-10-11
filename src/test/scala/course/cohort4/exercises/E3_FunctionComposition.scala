package course.cohort4.exercises

object E3_FunctionComposition extends App {

  /** ✏ EXERCISE
    *
    * Define the method `andThen`, which takes two functions, `f` and `g`, and
    * returns a new function that applies `g` to the result of applying `f`.
    *
    * You can think of this as building a pipeline of functions.
    */

  def andThen[A, B, C](f: A => B, g: B => C): A => C =
    (a: A) => g(f(a))

  val length: String => Int = str => {
    println(s"GETTING THE LENGTH OF $str")
    str.length
  }

  val isEven: Int => Boolean = int => {
    println(s"CHECKING IF $int IS EVEN")
    int % 2 == 0
  }

  val lengthIsEven: String => Boolean =
    isEven compose length

  println(lengthIsEven("ABABAB"))

  /** ✏ EXERCISE
    *
    * Define the method `compose`, which takes two functions, `f` and `g`, and
    * returns a new function that applies `f` to the result of applying `g`.
    *
    * This is sometimes written as f • g, or "f after g".
    */

  def compose[A, B, C](f: B => C, g: A => B): A => C = ???

  /** ✏ EXERCISE
    *
    * Define the method `curry`, which converts a function of type `(A, B) => C`
    * to a function of type `A => B => C`, the so-called "curried" version.
    */

  // burn the candle at both ends approach
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  val add: (Int, Int) => Int =
    (x: Int, y: Int) => x + y

  val curried: Int => Int => Int =
    curry(add)

  // add: (Int, Int) => Int
  // curried(10): Int => Int
  // f: Int => Int
  curried(10)(6) == add(10, 6)

  /** ✏ EXERCISE
    *
    * Define `uncurry`, which converts a function of type `A => B => C` to a
    * function of type `(A, B) => C`.
    */

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    ???

  /** ✏ EXERCISE
    *
    * Define the method `split`, which accepts two functions, `f` and `g`, which
    * both accept the same input type, and returns a new function that returns a
    * tuple of the results of applying `f` and `g` to the input.
    */
  def split[A, B, C](f: A => B, g: A => C): A => (B, C) =
    ???

  /** ✏ EXERCISE
    *
    * Define the method `both`, which accepts two functions, `f` and `g`, and
    * returns a function that operates on a tuple of their inputs.
    */
  def both[A, B, C, D](f: A => B, g: C => D): ((A, C)) => (B, D) =
    ???

}
