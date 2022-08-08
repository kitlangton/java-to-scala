package course.cohort3

import pprint.pprintln

import scala.annotation.tailrec

object Day6 extends App {
  val numsList = List(1, 2, 3, 4, 5, 6)

  // List[Int] -> Int
  // List[A] -> Collapse the list into a single summary value B
  // override def foldLeft[B](z: B)(op: (B, A) => B): B =
  // override def foldRight[B](z: B)(op: (A, B) => B): B =
  // ::(1, ::(2, ::(3, Nil)))
  // op(1, op(2, op(3, zero)))
  // 1. folds will visit every element of the list exactly once
  def filter[A](as: List[A])(p: A => Boolean): List[A] =
    as.foldRight(List.empty[A]) { (a, acc) =>
      if (p(a)) a :: acc
      else acc
    }

  //  pprintln(filter(numsList)(_ % 2 == 0))

  final case class Student(name: String, grade: Int)

  val students =
    List(
      Student("Kyle", 3),
      Student("Jane", 4),
      Student("Billy", 3),
      Student("Joe", 4),
      Student("Xavier", 5)
    )

  // - Chunking
  //   start: Hands applying force downwards to right side of wheel
  //          Feeling sweat pouring down head
  //          Pushing foot into resistant metal pedal
  //   end:   Pull into that driveway
  //
  //   start: All the syntax, [, (
  //   end: I have a list, I need a frequency map
  //
  // - List (Vector)
  // - Set
  // - Map
  //
  // groupBy: List[A] -> FreqMap
  // Input => Output
  //   groupBy
  // Input => FreqMap => Output
  val gradeMap: Map[Int, List[Student]] =
    students.groupBy(_.grade)
  //  pprintln(gradeMap)

  val gradeMap2: Map[Int, List[String]] =
    students.groupMap(_.grade)(_.name)
  //  pprintln(gradeMap2)

  val charFreqMap: Map[Char, Int] =
    "zzyyzzx".groupMapReduce(identity)(_ => 1)(_ + _)
  //  pprintln(charFreqMap)

  // vague mathematical concepts
  // partial function
  // Input -> Output
  // (Int, Int) => Int
  // (Int, 0)   => EXPLODE

  // PartialFunction

  // filter and map
  // remove elements from our collection
  // transform the type of the elements

  val pf: PartialFunction[Student, String] = { //
    case Student(name, 3) => name
  }

  println(pf.isDefinedAt(Student("Kit", 5)))

  sealed trait Event extends Product with Serializable

  object Event {
    case object Deleted extends Event

    final case class Added(time: Int) extends Event

    final case class Modified(time: Int) extends Event
  }

  val events =
    List(
      Event.Deleted,
      Event.Added(10),
      Event.Modified(20),
      Event.Deleted,
      Event.Added(5)
    )

  val result: List[Int] = events.collect {
    case Event.Added(time)    => time
    case Event.Modified(time) => time
  }
}

trait ==>[In, Out] {
  def apply(in: In): Out
}

trait MyPartialFunction[In, Out] extends ==>[In, Out] {
  def apply(in: In): Out

  def isDefinedAt(in: In): Boolean =
    try {
      apply(in)
      true
    } catch {
      case _: MatchError => false
    }
}

object Cool extends App {
  val stringLength: String ==> Int =
    new ==>[String, Int] {
      def apply(in: String): Int = in.length
    }

  println(stringLength("HELLO"))
}

object TailRecursionOne extends App {

  // if i > n:
  //  total
  // loop (total+1, i+1)
  // --> -> -> >
  def sum(n: Int): Int = {
    @tailrec
    def loop(total: Int, i: Int): Int =
      if (i > n) total
      else loop(total + 1, i + 1)

    loop(0, 0)
  }

  def sumWhile(n: Int): Int = {
    var total = 0
    var i     = 0
    while (i <= n) {
      total += i
      i += 1
    }
    total
  }

  println(sum(10))
}

object TailRecursionTwo extends App {
  val numbers = List(1, 2, 3, 4, 5, 6, 7)

  def sum(as: List[Int]): Int = {
    @tailrec
    def loop(as: List[Int], total: Int): Int =
      as match {
        case head :: tail => loop(tail, head + total)
        case Nil          => total
      }

    loop(as, 0)
  }
}

object AdventOfCodeTailRecursion extends App {
  val input1 = List(3, 3, 4, -2, -4)
  val input2 = List(-6, +3, +8, +5, -6)
  val input3 = List(+7, +7, -2, -7, -4)

  def solve(deltas: List[Int]): Int = {
    @tailrec
    def loop(ds: List[Int], seen: Set[Int], frequency: Int): Int =
      ds match {
        case _ if seen(frequency) => frequency
        case d :: tail            => loop(tail, seen + frequency, frequency + d)
        case Nil                  => loop(deltas, seen, frequency)
      }

    loop(deltas, Set.empty, 0)
  }

  pprintln(solve(input3))

}

object NullExploration extends App {
  // Nothing <: Null
  // None
  //           List[Int]
  //           List[Int] OR null
  //   Option[List[Int]]
  //      Some(List[Int]) OR None

  // A | null
  // Some(A) | None
  // Left(A) | Right(B)
  // A | B
  val someString: Option[String] = Some("hello")
  val noneString: Option[String] = None

  someString.getOrElse(12)

  // Int    >: Any
  // String >: Any
  val result: Either[String, Int] = someString.toLeft(10)
  pprintln(result)
  // Either

  var numbers: List[Int] = null

  def tweedledee(numbers: List[Int]): Unit = {
    val actualList = if (numbers eq null) List(1, 2, 3, 4, 5) else numbers
    tweedledum(actualList)
  }

  def tweedledum(numbers: List[Int]): Unit =
    println(numbers.length)

  tweedledee(numbers)
}

object OptionFromScratch extends App {

  sealed trait Option[+A] extends Product with Serializable { self =>

    def isDefined: Boolean = self match {
      case Some(_) => true
      case None    => false
    }

    def get: A = self match {
      case Some(value) => value
      case None        => throw new NoSuchElementException()
    }

    def getOrElse[A1 >: A](default: A1): A1 =
      self match {
        case Some(value) => value
        case None        => default
      }

  }

  final case class Some[A](value: A) extends Option[A]
  case object None                   extends Option[Nothing]

  sealed trait Event extends Product with Serializable

  object Event {
    final case class Added(int: Int)   extends Event
    final case class Removed(int: Int) extends Event
  }

  val stringOption: Option[Event.Added] =
    Some(Event.Added(10))

  //      Any
  // String  Int
//  private val result: Event.Added =
//    stringOption.getOrElse(Event.Removed(12))
//
//  println(result)
//  if (stringOption.isDefined)
//    println(stringOption.get)
//  else
//    println("NO SUCH ELEMENT DUDE")
}

// √ List Methods
//   - foldLeft
//   - groupBy
//   - find
//   - collect (PartialFunction)
// √ Tail Recursion
//   - Fibonacci (as promised!)
//   - Exercise: https://adventofcode.com/2018/day/1
// √ Option
// - Either
// - Parser
// - Implicit Conversions

object ImperativeFibonacci extends App {
  def fib(n: Int): Int = {
    @tailrec
    def loop(current: Int, next: Int, i: Int): Int =
      if (i > 0) loop(next, current + next, i - 1)
      else current

    loop(0, 1, n)
  }

  //   i 0 1 2 3 4
  // nex 1 1 2 3 5
  // cur 0 1 1 2 3
  def fibWhile(n: Int): Int = {
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

  (0 to 10).foreach(n => println(s"fib($n) = ${fibWhile(n)}"))
}
