package course.lessons

import cats.effect._
import cats.effect.std.Random

import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt

object CatsEffectSolutions_Forking extends IOApp.Simple {
  def makeFuture(value: Int): IO[Int] =
    IO.delay {
      println(s"START THREAD: ${Thread.currentThread().getName}")
      value
    }

  def flatMapAdd(task: IO[Int]): IO[Int] =
    task.flatMap { x =>
      println(s"THREAD: ${Thread.currentThread().getName}")
      IO.delay(x + 1)
    }

  @tailrec
  def repeat[A](n: Int, start: A, f: A => A): A =
    if (n == 0) start else repeat(n - 1, f(start), f)

  val run =
    repeat(500, makeFuture(1), flatMapAdd).void
}

object CatsEffectSolutions_ReferentialTransparency extends IOApp.Simple {
  def randomFuture(max: Int) =
    for {
      int <- Random.scalaUtilRandom[IO].flatMap(_.nextIntBounded(max))
      _   <- IO.println(s"START $int")
      _   <- IO.sleep(300.millis)
    } yield int

  val run = for {
    x <- randomFuture(100)
    y <- randomFuture(100)
    z <- randomFuture(100)
  } yield println(s"$x + $y + $z = ${x + y + z}")

}

object CatsEffectSolutions_FailureInterruption extends IOApp.Simple {

  def delay(name: String, expr: => Int, ms: Int): IO[Int] =
    for {
      _      <- IO.println(s"START $name")
      result <- IO.delay(expr).delayBy(ms.millis).onCancel(IO.println(s"INTERRUPTED $name"))
      _      <- IO.println(s"END $name")
    } yield result

  private val result: IO[List[Int]] = IO
    .parSequenceN(8)(
      List(
        delay("boom", throw new Exception("Boom!"), 1500),
        delay("one", 1, 1000),
        delay("two", 2, 2000),
        delay("three", 3, 3000)
      )
    )

  val run = result.attempt.flatMap(result => IO.println(result)).void

}

object CatsEffectSolutions_RacingInterruption extends IOApp.Simple {
  def delay(name: String, expr: => Int, ms: Int): IO[Int] =
    for {
      _      <- IO.println(s"START $name")
      result <- IO.delay(expr).delayBy(ms.millis).onCancel(IO.println(s"INTERRUPTED $name"))
      _      <- IO.println(s"END $name")
    } yield result

  val result: IO[Either[Either[Int, Int], Int]] =
    delay("one", 1, 1000) race
      delay("two", 2, 2000) race
      delay("three", 3, 3000)

  val run = result.flatMap(result => IO.println(s"RESULT $result")).void
}

object CatsEffectSync_StackTrace extends App {
  def methodOne(): Int =
    methodTwo(1)

  def methodTwo(int: Int): Int =
    methodThree(int + 2)

  def methodThree(int: Int): Int =
    methodFour(int + 3)

  def methodFour(i: Int): Int =
    throw new Exception(s"Boom $i!")

  methodOne()
}

object CatsEffectSolutions_StackTraces extends IOApp.Simple {
  def delay(name: String, expr: => Int, ms: Int): IO[Int] =
    for {
      _      <- IO.println(s"START $name")
      result <- IO.delay(expr).delayBy(ms.millis)
      _      <- IO.println(s"END $name")
    } yield result

  val methodOne: IO[Int] =
    delay("one", 1, 500).flatMap { result =>
      methodTwo(result)
    }

  def methodTwo(int: Int): IO[Int] =
    delay("two", int + 2, 500).flatMap { result =>
      methodThree(result)
    }

  def methodThree(int: Int): IO[Int] =
    delay("three", int + 3, 500).flatMap { result =>
      methodFour(result)
    }

  def methodFour(i: Int): IO[Int] =
    delay("four", throw new Exception(s"Boom $i!"), 500)

  val run = methodOne.void
}

//object CatsEffectSolutions_ResourceSafety extends App {
//  trait Connection {
//    def close(): Unit
//    def send(msg: String): Unit
//  }
//
//  object Connection {
//    val openConnections = new AtomicInteger(0)
//
//    def make: Connection = {
//      openConnections.getAndIncrement()
//      new Connection {
//        def close(): Unit = {
//          println("Closing connection")
//          openConnections.getAndDecrement()
//        }
//
//        def send(msg: String): Unit =
//          println(s"Sending: $msg")
//      }
//    }
//  }
//
//  def openConnection: IO[Connection] =
//    IO {
//      println(s"OPENING CONNECTION")
//      Thread.sleep(500)
//      Connection.make
//    }
//
//  val future = {
//    for {
//      conn <- openConnection
//      _ <- IO {
//             println(s"SENDING MESSAGE")
//             Thread.sleep(1000)
//             throw new Error("OOPS")
//             conn.send("Hello")
//           }
//      _ <- IO(conn.close())
//    } yield println("DONE")
//  }
//
//  println(Await.result(future, scala.concurrent.duration.Duration.Inf))
//}
