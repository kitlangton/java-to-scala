package course.archive.cohort1.m1_basics

import scala.annotation.tailrec

/**   - Immutable Data
  *     - Case Classes
  *       - A AND B AND C
  *       - Person
  *         - Name AND Age AND SocialSecurityNumber
  *       - User
  *         - UUID AND Email AND PasswordHash
  *     - Sealed Traits
  *       - A OR B OR C
  *       - Option[A]
  *         - Some(A) OR None
  *       - Either
  *         - Left(E) OR Right(A)
  *       - Popcorn Size
  *         - Small OR Medium OR Large
  */

// - Option from Scratch
// - Variance
//   - Option is a CONTAINER or PRODUCER of the type A
// A => B
// Option[B]

//sealed trait Option[+A] {
//  import Option._
//
//  def orElse[A1 >: A](that: Option[A1]): Option[A1] =
//    this match {
//      case Some(value) => Some(value)
//      case None        => that
//    }
//
//  def map[B](f: A => B): Option[B] =
//    this match {
//      case Some(a) => Some(f(a))
//      case None    => None
//    }
//
//  def isEmpty: Boolean =
//    this match {
//      case Some(_) => false
//      case None    => true
//    }
//
//  def nonEmpty: Boolean = !isEmpty
//}
//
//object Option {
//  final case class Some[A](value: A) extends Option[A]
//  final case object None             extends Option[Nothing]
//}
//
//sealed trait Either[+E, +A] {
//  import Either._
//
//  def map[B](f: A => B): Either[E, B] =
//    this match {
//      case Left(e)  => Left(e)
//      case Right(a) => Right(f(a))
//    }
//
//  def mapError[E2](f: E => E2): Either[E2, A] =
//    this match {
//      case Left(e)  => Left(f(e))
//      case Right(a) => Right(a)
//    }
//}
//
//object Either {
//  final case class Right[A](value: A) extends Either[Nothing, A]
//  final case class Left[E](value: E)  extends Either[E, Nothing]
//}

// - List[Int] from Scratch
//   - Nil
//   OR
//   - :: A AND List[A]

//trait GreaterThan[A, B]
//
//object What {
//  val int: String GreaterThan Int    = ???
//  val int2: GreaterThan[String, Int] = ???
//}

//sealed trait List[+A] {
//  import List._
//
//  def headOption: Option[A] =
//    this match {
//      case Nil       => None
//      case head :: _ => Some(head)
//    }
//
//  def head2Option: Option[(A, A)] =
//    this match {
//      case a :: a2 :: _ => Some((a, a2))
//      case _            => None
//    }
//
//  def map[B](f: A => B): List[B] =
//    this match {
//      case Nil       => Nil
//      case ::(a, as) => f(a) :: as.map(f)
//    }
//
//  def take(n: Int): List[A] =
//    this match {
//      case head :: tail if n > 0 =>
//        head :: tail.take(n - 1)
//      case _ => Nil
//    }
//
//  // List(1...10000).drop(1)
//
//  def drop(n: Int): List[A] =
//    this match {
//      case _ :: tail if n > 0 =>
//        tail.drop(n - 1)
//      case _ => this
//    }
//
//  // Chain
//  val takeWhileTest = test("takeWhile") {
//    def takeWhile[A](list: List[A])(pred: A => Boolean): List[A] = {
//      val init: (Boolean, List[A]) = (true, List.empty[A])
//
//      val res = list.foldLeft(init) {
//        case ((true, l), next) if pred(next) =>
//          (true, next :: l)
//
//        case ((_, l), _) =>
//          (false, l)
//      }
//
//      res._2.reverse
//    }
//
//    assertTrue(takeWhile(List(1, 3, 7, 3))(_ < 5) == List(1, 3))
//  }
//  // Chunk
//
//  // The list is empty
//  // 3 :: 2 :: 1 :: Nil
//  // f(Z, 3) :: 2 :: 1 :: Nil
//  // Z1      :: 2 :: 1 :: Nil
//  // f(Z1     , 2) :: 1 :: Nil
//  // Z            :: 1 :: Nil
//  // ..
//  // Z
//  // 1 :: 2 :: 3 :: Nil
//  // add(add(add(0, 1), 2, 3)
//  def foldLeft[Z](z: Z)(f: (Z, A) => Z): Z =
//    this match {
//      case Nil       => z
//      case ::(a, as) => as.foldLeft(f(z, a))(f)
//    }
//
//  // add(1, add(2, add(3, 0)))
//  def foldRight[Z](z: Z)(f: (A, Z) => Z): Z =
//    this match {
//      case Nil => z
//      case head :: tail =>
//        f(head, tail.foldRight(z)(f))
//    }
//
//  // Methods that end with a :, become right-associative
//  def ::[A1 >: A](a: A1): List[A1] =
//    List.::(a, this)
//}
//
//// Algebraic Data Type
//object List {
//  case object Nil                          extends List[Nothing]
//  case class ::[A](head: A, tail: List[A]) extends List[A]
//}

object ListExamples extends App {
  import List._

  val listInt: List[Int]  = Nil
  val listInt1: List[Int] = 1 :: Nil
  val listInt2: List[Int] = 2 :: 1 :: Nil
  val listInt3: List[Int] = 3 :: 2 :: 1 :: Nil

  // List[Int] -> Z

//  val (left, right) = (10, "hello")
//  val head :: tail  = listInt3
//  listInt3 match {
//    case three :: two :: tail     => println(s"$three, $two, $tail")
//    case ::(three, ::(two, tail)) => println(s"$three, $two, $tail")
//  }
//
//  println(listInt.headOption)
//  println(listInt1.headOption)
//  println(listInt2.headOption)
}

final case class Pet(name: String, age: Int)
final case class Person(pet: Pet, name: String)

/** # Fibonacci Number Definition
  * https://en.wikipedia.org/wiki/Fibonacci_number#Definition
  *
  * The value of the nth Fibonacci number is defined as the sum of the two
  * previous Fibonacci numbers.
  *
  *   - fib(0) = 0
  *   - fib(1) = 1
  *   - fib(n) = fib(n-1) + fib(n-2)
  */
object RecursiveFibonacci extends App {
  def fib(n: Int): Int =
    n match {
      case 0 => 0
      case 1 => 1
      case n => fib(n - 1) + fib(n - 2)
    }

  (0 to 10).foreach(n => println(s"fib($n) = ${fib(n)}"))
  fib(1000)
}

object ImperativeFibonacci extends App {
  def fib(n: Int): Int = {
    var current = 0
    var next    = 1
    var i       = n
    while (i > 0) {
      val temp = next
      next = current + next
      current = temp
      i -= 1
    }
    current
  }

  (0 to 10).foreach(n => println(s"fib($n) = ${fib(n)}"))
}

object TailRecursiveFibonacci extends App {
  def fib(n: Int): Int = {
    @tailrec
    def loop(current: Int, next: Int, i: Int): Int =
      if (i > 0) loop(next, current + next, i - 1)
      else current

    loop(0, 1, n)
  }

  (0 to 10).foreach(n => println(s"fib($n) = ${fib(n)}"))
  println(fib(10000000))
}

/** Partitioning Students
  */
final case class Student(name: String, grade: Int)
//
//object Student {
//  val allStudents =
//    List(
//      Student("Charlie", 8),
//      Student("Eve", 68),
//      Student("Fred", 50),
//      Student("Fitzpatrick", 77),
//      Student("David", 77),
//      Student("Bob", 82),
//      Student("Alice", 100),
//      Student("Bobo The Clown", 45)
//    )
//}
//
//object ImperativePartitionStudents extends App {
//  def partitionStudents(students: List[Student]): (List[Student], List[Student]) = {
//    var passing   = List.empty[Student]
//    var failing   = List.empty[Student]
//    var remaining = students
//    while (remaining.nonEmpty) {
//      val student = remaining.head
//      if (student.grade >= 60)
//        passing = student :: passing
//      else
//        failing = student :: failing
//      remaining = remaining.tail
//    }
//    (passing, failing)
//  }
//
//  val (passingStudents, failingStudents) =
//    partitionStudents(Student.allStudents)
//
//  println(s"Passing students: $passingStudents")
//  println(s"Failing students: $failingStudents")
//}
//
//object RecursivePartitionStudents extends App {
//  def partitionStudentsImperative(students: List[Student]): (List[Student], List[Student]) = {
//    var passing   = List.empty[Student]
//    var failing   = List.empty[Student]
//    var remaining = students
//    while (remaining.nonEmpty) {
//      val student = remaining.head
//      if (student.grade >= 60)
//        passing = student :: passing
//      else
//        failing = student :: failing
//      remaining = remaining.tail
//    }
//    (passing, failing)
//  }
//
//  def partition[A](list: List[A])(pred: A => Boolean): (List[A], List[A]) = {
//    val (as, bs) =
//      list.foldLeft((List.empty[A], List.empty[A])) { //
//        case ((successes, fails), a) =>
//          if (pred(a))
//            (a :: successes, fails)
//          else
//            (successes, a :: fails)
//      }
//
//    (as.reverse, bs.reverse)
//  }
//
//  def partitionStudents(students: List[Student]): (List[Student], List[Student]) = {
//    @tailrec
//    def loop(students: List[Student], passing: List[Student], failing: List[Student]): (List[Student], List[Student]) =
//      students match {
//        case student :: students if student.grade >= 60 =>
//          loop(students, student :: passing, failing)
//        case student :: students =>
//          loop(students, passing, student :: failing)
//        case Nil =>
//          (passing, failing)
//      }
//
//    loop(students, Nil, Nil)
//  }
//
//  val (passingStudents, failingStudents) =
//    partitionStudents(Student.allStudents)
//
//  println(s"Passing students: $passingStudents")
//  println(s"Failing students: $failingStudents")
//}
//
//object StandardLibraryPartitionStudents extends App {
//  def partitionStudents(students: List[Student]): (List[Student], List[Student]) =
//    students.partition(_.grade >= 60)
//
//  val (passingStudents, failingStudents) =
//    partitionStudents(Student.allStudents)
//
//  println(s"Passing students: $passingStudents")
//  println(s"Failing students: $failingStudents")
//}
