package course.archive.cohort3.projects

trait MyFunction[In, Out] {
  def apply(in: In): Out
}

object Cool extends App {
  val lengthFunction =
    new MyFunction[String, Int] {
      override def apply(string: String): Int = string.length
    }

  val myLength = (string: String) => string.length

  println(lengthFunction("HELLO"))
}

// 1. Predicate
// 2. EmailFilter some sort of Fraud Detection
// 3. Pipeline

// The Red Book
// Random and Property Based Testing
// Parser Combinators

// 1. Subsuming via foldLeft
// toString
// reverse
// average
// sum

// 2. MapReduce
// One Pass Average case class
// Connect to distributed computation
// Spark Source Code
