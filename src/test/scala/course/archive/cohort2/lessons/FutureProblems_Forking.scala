package course.lessons

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

object FutureProblems_Forking extends App {
  def makeFuture(value: Int): Future[Int] =
    Future {
      println(s"START THREAD: ${Thread.currentThread().getName}")
      value
    }

  def flatMapAdd(fut: Future[Int]): Future[Int] =
    fut.flatMap { x =>
      println(s"THREAD: ${Thread.currentThread().getName}")
      Future(x + 1)
    }

  @tailrec
  def repeat[A](n: Int, start: A, f: A => A): A =
    if (n == 0) start else repeat(n - 1, f(start), f)

  val mapped = repeat(500, makeFuture(1), flatMapAdd)

  println(Await.result(mapped, scala.concurrent.duration.Duration.Inf))
}

object FutureProblems_ReferentialTransparency extends App {
  def randomFuture(max: Int) = Future {
    val randomInt = scala.util.Random.nextInt(max)
    println(s"START $randomInt")
    Thread.sleep(300)
    randomInt
  }

  for {
    x <- randomFuture(100)
    y <- randomFuture(100)
    z <- randomFuture(100)
  } yield println(s"$x + $y + $z = ${x + y + z}")

  Thread.sleep(3000)
}

object FutureProblems_FailureInterruption extends App {
  def delay(value0: => Int, ms: Int): Future[Int] =
    Future {
      val value = value0
      println(s"START $value")
      Thread.sleep(ms)
      println(s"FINISHED CALCULATING $value")
      value
    }

  delay(1, 1000).onComplete(result => println(result))

  val result: Future[List[Int]] = Future.sequence(
    List(
      delay(throw new Exception("Boom!"), 100),
      delay(1, 1000),
      delay(2, 2000),
      delay(3, 3000)
    )
  )

  result.onComplete { result =>
    println(s"RESULT: $result")
  }

  Thread.sleep(3000)
}

object FutureProblems_RacingInterruption extends App {
  def delay(value0: => Int, ms: Int): Future[Int] =
    Future {
      val value = value0
      println(s"START $value")
      Thread.sleep(ms)
      println(s"COMPLETED $value")
      value
    }

  val result: Future[Int] = Future.firstCompletedOf(
    List(
      delay(1, 1000),
      delay(2, 2000),
      delay(3, 3000)
    )
  )

  result.onComplete { result =>
    println(s"RESULT: $result")
  }

  Thread.sleep(3000)
}

object Sync_StackTrace extends App {
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

object FutureProblems_StackTraces extends App {
  def delay(value0: => Int, ms: Int): Future[Int] =
    Future {
      val value = value0
      println(s"START $value")
      Thread.sleep(ms)
      value
    }

  def methodOne(): Future[Int] =
    delay(1, 500).flatMap { result =>
      methodTwo(result)
    }

  def methodTwo(int: Int): Future[Int] =
    delay(int + 2, 500).flatMap { result =>
      methodThree(result)
    }

  def methodThree(int: Int): Future[Int] =
    delay(int + 3, 500).flatMap { result =>
      methodFour(result)
    }

  def methodFour(i: Int): Future[Int] =
    delay(throw new Exception(s"Boom $i!"), 500)

  val result = methodOne()

  println(Await.result(result, scala.concurrent.duration.Duration.Inf))
}

object FutureProblems_ResourceSafety extends App {
  trait Connection {
    def close(): Unit
    def send(msg: String): Unit
  }

  object Connection {
    val openConnections = new AtomicInteger(0)

    def make: Connection = {
      openConnections.getAndIncrement()
      new Connection {
        def close(): Unit = {
          println("Closing connection")
          openConnections.getAndDecrement()
        }

        def send(msg: String): Unit =
          println(s"Sending: $msg")
      }
    }
  }

  def openConnection: Future[Connection] =
    Future {
      println(s"OPENING CONNECTION")
      Thread.sleep(500)
      Connection.make
    }

  val future = {
    for {
      conn <- openConnection
      _ <- Future {
             println(s"SENDING MESSAGE")
             Thread.sleep(1000)
             throw new Error("OOPS")
             conn.send("Hello")
           }
      _ <- Future(conn.close())
    } yield println("DONE")
  }

  println(Await.result(future, scala.concurrent.duration.Duration.Inf))
}
