package course.exercises

object E3_FunctionComposition extends App {

  /** ✏ EXERCISE
    *
    * Define the method `andThen`, which takes two functions, `f` and `g`, and
    * returns a new function that applies `g` to the result of applying `f`.
    *
    * You can think of this as building a pipeline of functions.
    */

  def andThen[A, B, C](f: A => B, g: B => C): A => C =
    ???

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

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    ???

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

  def both[A, B, C, D](f: A => B, g: C => D): (A, B) => (C, D) =
    ???

}
