package course.cohort3

import pprint.pprintln

import scala.annotation.tailrec
import scala.language.implicitConversions

object Day8 extends App {
  // - Functional Design
  // - Parser
  // - Implicit Conversions
  pprintln("HELLO WORLD!")
}

sealed trait TicTacToePosition extends Product with Serializable

object TicTacToePosition {
  case object TopLeft   extends TicTacToePosition
  case object TopMiddle extends TicTacToePosition
  case object TopRight  extends TicTacToePosition

  case object CenterLeft   extends TicTacToePosition
  case object CenterMiddle extends TicTacToePosition
  case object CenterRight  extends TicTacToePosition

  case object BottomLeft   extends TicTacToePosition
  case object BottomMiddle extends TicTacToePosition
  case object BottomRight  extends TicTacToePosition

  val grid = List(
    List(TopLeft, TopMiddle, TopRight),
    List(CenterLeft, CenterMiddle, CenterRight),
    List(BottomLeft, BottomMiddle, BottomRight)
  )

}

import TicTacToePosition._

sealed trait Player extends Product with Serializable

object Player {
  case object X extends Player
  case object O extends Player
}

object TicTacToe extends App {

  // 9 < x
  // (123, 588) -> (2, 2)
  // (Int, Int)

  val board =
    Map(
      TopLeft      -> Player.X,
      BottomRight  -> Player.O,
      TopRight     -> Player.X,
      TopMiddle    -> Player.O,
      CenterMiddle -> Player.X
    )

  def renderBoard(board: Map[TicTacToePosition, Player]): String =
    TicTacToePosition.grid
      .map { row =>
        row.map { position =>
          board.get(position) match {
            case Some(player) => player.toString
            case None         => "_"
          }
        }.mkString
      }
      .mkString("\n")

  println(renderBoard(board))

}

object ExampleTicTac extends App {
  case class TicTacToePosition(row: Index3, column: Index3)

  sealed trait Index3

  object Index3 {
    case object Index1 extends Index3
    case object Index2 extends Index3
    case object Index3 extends Index3
  }
}

trait Parser[+A] { self =>

  def repeat: Parser[List[A]] =
    (input: String) => {
      @tailrec
      def loop(remainder: String, acc: List[A]): (String, List[A]) =
        self.parseImpl(remainder) match {
          case Some((remainder, a)) => loop(remainder, a :: acc)
          case None                 => (remainder, acc.reverse)
        }

      Some(loop(input, List.empty))
    }

  def parse(input: String): Option[A] =
    parseImpl(input).map(_._2)

  def parseImpl(input: String): Option[(String, A)]

  // (Int, String) -> String
  // -> String
  // the remaining input
  def ~[B](that: Parser[B]): Parser[(A, B)] = (input: String) =>
    for {
      (remainder, a) <- self.parseImpl(input)
      (remainder, b) <- that.parseImpl(remainder)
    } yield (remainder, (a, b))

  def ~>[B](that: Parser[B]): Parser[B] = (self ~ that).map(_._2)
  def <~[B](that: Parser[B]): Parser[A] = (self ~ that).map(_._1)

  def map[B](f: A => B): Parser[B] = (input: String) =>
    self.parseImpl(input).map { case (remainder, a) =>
      (remainder, f(a))
    }

  def flatMap[B](f: A => Parser[B]): Parser[B] = (input: String) =>
    self.parseImpl(input) match {
      case Some((remainder, a)) =>
        val parserB = f(a)
        parserB.parseImpl(remainder)
      case None => None
    }
}

object Parser extends App {
  final case class Coord(x: Int, y: Int)

  def charIn(chars: String): Parser[Char] = new Parser[Char] {
    val charSet: Set[Char] = chars.toSet

    override def parseImpl(input: String): Option[(String, Char)] =
      input.headOption.flatMap { char =>
        if (charSet.contains(char)) Some((input.tail, char)) else None
      }
  }

  val abcParser = charIn("abc")

  lazy val intParser: Parser[Int] = (input: String) => {
    val (prefix, remaining) = input.span(_.isDigit)
    if (prefix.isEmpty) None
    else Some((remaining, prefix.toInt))
  }

  implicit def string2Parser(string: String): Parser[Char] =
    charIn(string)

  // 1. String => Parser
  val cool = intParser ~ "x"

  // Option / Either / Parser
  val coordParser: Parser[Coord] =
    for {
      _ <- charIn("(")
      x <- intParser
      _ <- charIn("x")
      y <- intParser
      _ <- charIn(")")
    } yield Coord(x, y)

  lazy val coordsParser: Parser[List[Coord]] =
    coordParser.repeat

  val example =
    "(123x18)(5x5)(123x5)(888x888)".stripMargin.trim

  pprintln(coordsParser.parse(example))
  // "500x900\n300x300" -> List[Position]

}

object SyncCode extends App {
  def getSync(url: String): String =
    scala.io.Source.fromURL(url).mkString

  println("HELLO")
  println(getSync("https://wikipedia.com")) // 1
  println("DONE")

}

object ProcessAsync extends App {
  def getSync(url: String): String =
    scala.io.Source.fromURL(url).mkString

  def processUsers(string: String): Unit = {
    println(s"processUsers START: GOT STRING OF LENGTH ${string.length}")
    Thread.sleep(1000)
    println(s"processUsers DONE: GOT STRING OF LENGTH ${string.length}")
  }

  def stuff() = {
    println("START SOME STUFF")
    Thread.sleep(3000)
    println("DONE SOME STUFF")
  }

  new Thread(() => {
    println("GET AND PROCESS USERS")
    val users = getSync("https://wikipedia.com")
    processUsers(users)
  }).start()

  println("OTHER IMPORTANT STUFF")
  stuff()

}

object ExampleForComp extends App {

  def a1(): Option[Int] = Some(12)
  def b2(int: Int): Option[Int] =
    if (int % 2 == 0) Some(12) else None
  def c3(int: Int): Option[Int] =
    Some(int * 2)

  implicit final class OptionOps[A](private val self: Option[A]) extends AnyVal {
    def tapNone(f: => Unit): Option[A] =
      self match {
        case Some(value) =>
          Some(value)
        case None =>
          f
          None
      }
  }

  // Futures with Option
  // Future[Option[A]]
  // Future[Either[E, A]]
  // IO[AppError, Int]
  // b2(a).tapError {
  //   case MySubError => emitMetric
  // }

  val result = for {
    a <- a1()
    b <- b2(a).tapNone(println(s"GOT NONE FOR $a input"))
    c <- c3(b)
  } yield {
    println(a, b, c)
    (a, b, c)
  }
  pprintln(result)
  //super awesome code that has no flaws... i promise

}
