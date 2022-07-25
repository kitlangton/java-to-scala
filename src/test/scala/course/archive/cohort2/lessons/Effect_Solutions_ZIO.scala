package course.lessons

import scala.annotation.tailrec
import zio._

object ZIOSolutions_Forking extends ZIOAppDefault {
  def makeZIO(value: Int): Task[Int] =
    ZIO.attempt {
      println(s"START THREAD: ${Thread.currentThread().getName}")
      value
    }

  def flatMapAdd(task: Task[Int]): Task[Int] =
    task.flatMap { x =>
      println(s"THREAD: ${Thread.currentThread().getName}")
      ZIO.succeed(x + 1)
    }

  @tailrec
  def repeat[A](n: Int, start: A, f: A => A): A =
    if (n == 0) start else repeat(n - 1, f(start), f)

  val run =
    repeat(500, makeZIO(1), flatMapAdd)
}

object ZIOSolutions_ReferentialTransparency extends ZIOAppDefault {
  def randomFuture(max: Int): Task[Int] =
    for {
      int <- Random.nextIntBounded(max)
      _   <- ZIO.debug(s"START $int")
      _   <- ZIO.sleep(300.millis)
    } yield int

  val run = for {
    x <- randomFuture(100)
    y <- randomFuture(100)
    z <- randomFuture(100)
  } yield println(s"$x + $y + $z = ${x + y + z}")
}

object ZIOSolutions_FailureInterruption extends ZIOAppDefault {

  def delay(name: String, expr: => Int, ms: Int): Task[Int] =
    for {
      _      <- ZIO.debug(s"START $name")
      result <- ZIO.attempt(expr).delay(ms.millis).onInterrupt(ZIO.debug(s"INTERRUPTED $name"))
      _      <- ZIO.debug(s"END $name")
    } yield result

  private val result: Task[List[Int]] = ZIO
    .collectAllPar(
      List(
        delay("boom", throw new Exception("Boom!"), 1500),
        delay("one", 1, 1000),
        delay("two", 2, 2000),
        delay("three", 3, 3000)
      )
    )

  val run = result.either.debug("RESULT")

}

object ZIOSolutions_RacingInterruption extends ZIOAppDefault {
  def delay(name: String, expr: => Int, ms: Int): Task[Int] =
    for {
      _      <- ZIO.debug(s"START $name")
      result <- ZIO.attempt(expr).delay(ms.millis).onInterrupt(ZIO.debug(s"INTERRUPTED $name"))
      _      <- ZIO.debug(s"END $name")
    } yield result

  val result: Task[Int] =
    delay("one", 1, 1000) race
      delay("two", 2, 2000) race
      delay("three", 3, 3000)

  val run = result.debug("RESULT")
}

object AnotherSync_StackTrace extends App {
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

object ZIOSolutions_StackTraces extends ZIOAppDefault {
  def delay(name: String, expr: => Int, ms: Int)(implicit trace: Trace): Task[Int] =
    for {
      _      <- ZIO.debug(s"START $name")
      result <- ZIO.attempt(expr).delay(ms.millis)
      _      <- ZIO.debug(s"END $name")
    } yield result

  val methodOne: Task[Int] =
    for {
      x      <- delay("one", 1, 500)
      result <- methodTwo(x)
    } yield result

  def methodTwo(int: Int): Task[Int] =
    for {
      y      <- delay("two", int + 2, 1000)
      result <- methodThree(y)
    } yield result

  def methodThree(int: Int): Task[Int] =
    for {
      z      <- delay("three", int + 3, 1500)
      result <- methodFour(z)
    } yield result

  def methodFour(i: Int): Task[Int] =
    delay("four", throw new Exception(s"Boom $i!"), 500)

  val run = methodOne
}

//object ZIOSolutions_ResourceSafety extends App {
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
//  def openConnection: Task[Connection] =
//    Task {
//      println(s"OPENING CONNECTION")
//      Thread.sleep(500)
//      Connection.make
//    }
//
//  val future = {
//    for {
//      conn <- openConnection
//      _ <- Task {
//             println(s"SENDING MESSAGE")
//             Thread.sleep(1000)
//             throw new Error("OOPS")
//             conn.send("Hello")
//           }
//      _ <- Task(conn.close())
//    } yield println("DONE")
//  }
//
//  println(Await.result(future, scala.concurrent.duration.Duration.Inf))
//}
