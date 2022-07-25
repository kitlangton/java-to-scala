package course.lessons

import course.lessons.ClassyStuff.className

import scala.annotation.tailrec
import scala.reflect.ClassTag

trait Parser[+A] { self =>

  def seperatedBy(sep: Parser[Any]): Parser[List[A]] =
    new Parser[List[A]] {
      override def parseImpl(input: String): Option[(String, List[A])] = {
        @tailrec
        def loop(input: String, acc: List[A]): (String, List[A]) =
          (sep zip self).parseImpl(input) match {
            case Some((input, (_, a))) => loop(input, a :: acc)
            case None                  => input -> acc.reverse
          }

        self.parseImpl(input) match {
          case Some((leftovers, a)) =>
            Some(loop(leftovers, List(a)))
          case None =>
            Some(input -> List.empty)
        }
      }
    }

  // leftovers
  def parseImpl(input: String): Option[(String, A)]

  def parse(input: String): Option[A] = parseImpl(input).map(_._2)

  def map[B](f: A => B): Parser[B] = new Parser[B] {
    override def parseImpl(input: String): Option[(String, B)] =
      self
        .parseImpl(input)
        .map { case (str, a) => (str, f(a)) }
  }

  def zip[B](that: Parser[B]): Parser[(A, B)] =
    new Parser[(A, B)] {
      override def parseImpl(input: String): Option[(String, (A, B))] =
        for {
          (leftovers, a) <- self.parseImpl(input)
          (leftovers, b) <- that.parseImpl(leftovers)
        } yield leftovers -> (a, b)
    }
}

object Parser extends App {
//  def zip[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] = ???

  val char: Parser[Char] = new Parser[Char] {
    override def parseImpl(input: String): Option[(String, Char)] =
      input.headOption match {
        case Some(char) => Some(input.tail -> char)
        case None       => None
      }
  }

  def string(value: String): Parser[String] = new Parser[String] {
    override def parseImpl(input: String): Option[(String, String)] =
      if (input.startsWith(value)) Some(input.drop(value.length) -> value)
      else None
  }

  val digit = new Parser[Int] {
    override def parseImpl(input: String): Option[(String, Int)] =
      input.headOption match {
        case Some(char) if char.isDigit => Some(input.tail -> char.toString.toInt)
        case _                          => None
      }
  }

  //   (char zip digit): Parser[(Char, Int)]
  //   "a1c" -> Some(('a', 1))
  //   "a1" -> Some(('a', 1))
  //   "a" -> None
  //   "1c" -> None
  //   "" -> None
  val composed: Parser[(Char, Int)] = char zip digit

  final case class ChessPosition(row: Char, col: Int)

  val chessPositionParser: Parser[ChessPosition] =
    composed.map(ChessPosition.tupled)

  final case class ChessMove(from: ChessPosition, to: ChessPosition)

  // string(" to ")
  val chessMoveParser: Parser[ChessMove] =
    (chessPositionParser zip string(" to ") zip chessPositionParser)
      .map { case ((p1, _), p2) =>
        ChessMove(p1, p2)
      }

  val chessMovesParser: Parser[List[ChessMove]] =
    chessMoveParser.seperatedBy(string("\n"))

  val inputExample =
    """
A1 to A2
B1 to E4
C7 to D1
C8 to D1
C2 to E1
C7 to D1
  """.trim

  // Some(('1', 1))
  println(chessMovesParser.parse(inputExample).map(_.mkString("\n")))
}

// composition
// - constructors (simple solutions)
//

// - combinators (solution combiners)
//   p1.op(p2)
//   p1.transform
//
//   digit.repeat: Parser[List[Int]]
//   "123"   -> Some(List(1, 2, 3))
//   "1234a" -> Some(List(1, 2, 3, 4))
//   "anth"  -> Some(List())
//
object ClassyStuff extends App {
  def className[A](implicit tag: ClassTag[A]): Unit = {
    println(tag)
    println(tag.runtimeClass)
    println(tag.runtimeClass.getName)
  }

  final case class Person[A](value: A)
  className[Person[Int]]
}
