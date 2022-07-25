package course.lessons

import cats.effect.{IO => CIO, IOApp => CIOApp, Trace => _, Ref => CRef, _}
import course.lessons.Utils._
import zio._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Future}

// THREAD AFFINITY
// DAY 8
object ThreadAffinity_Future extends App {
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

  val mapped = repeat(500, makeFuture(1), flatMapAdd)

  println(Await.result(mapped, scala.concurrent.duration.Duration.Inf))
}

object ThreadAffinity_ZIO extends ZIOAppDefault {
  def makeZIO(value: Int): UIO[Int] =
    ZIO.succeed {
      println(s"START THREAD: ${Thread.currentThread().getName}")
      value
    }

  def flatMapAdd(zio: UIO[Int]): UIO[Int] =
    zio.flatMap { x =>
      println(s"THREAD: ${Thread.currentThread().getName}")
      ZIO.succeed(x + 1)
    }

  val run =
    repeat(5000, makeZIO(1), flatMapAdd)
}

object ThreadAffinity_CatsEffect extends CIOApp.Simple {
  def makeFuture(value: Int): CIO[Int] =
    CIO.delay {
      println(s"START THREAD: ${Thread.currentThread().getName}")
      value
    }

  def flatMapAdd(task: CIO[Int]): CIO[Int] =
    task.flatMap { x =>
      println(s"THREAD: ${Thread.currentThread().getName}")
      CIO.pure(x + 1)
    }

  val run =
    repeat(5000, makeFuture(1), flatMapAdd).void
}

// REFERENTIAL TRANSPARENCY

object ReferentialTransparency_Future extends App {
  def randomFuture(max: Int): Future[Int] = Future {
    val randomInt = scala.util.Random.nextInt(max)
    println(s"START $randomInt")
    Thread.sleep(1000)
    randomInt
  }

  private val eventualInt: Future[Int] = randomFuture(100)

  for {
    x <- eventualInt
    y <- eventualInt
    z <- eventualInt
  } yield println(s"$x + $y + $z = ${x + y + z}")

  Thread.sleep(4000)
}

object ReferentialTransparency_ZIO extends ZIOAppDefault {
  def randomZIO(max: Int): UIO[Int] =
    for {
      int <- Random.nextIntBounded(max)
      _   <- ZIO.debug(s"START $int")
      _   <- ZIO.sleep(500.millis)
    } yield int

  // BLUEPRINT/DESCRIPTION/RECIPE
//  private val random100: UIO[Int] =
//    randomZIO(100)

  val run = for {
    random100 <- randomZIO(100).cached(1.second)
    x         <- random100
    y         <- random100
    _         <- ZIO.sleep(1.second)
    z         <- random100
  } yield println(s"$x + $y + $z = ${x + y + z}")
}

object ReferentialTransparency_CatsEffect extends CIOApp.Simple {
  def randomCIO(max: Int): CIO[Int] =
    for {
      int <- std.Random.scalaUtilRandom[CIO].flatMap(_.nextIntBounded(max))
      _   <- CIO.println(s"START $int")
      _   <- CIO.sleep(300.millis)
    } yield int

//  private val random100: CIO[Int] =
//    randomCIO(100)

  val run = for {
    random100 <- randomCIO(100).memoize
    x         <- random100
    y         <- random100
    z         <- random100
  } yield println(s"$x + $y + $z = ${x + y + z}")
}

// INTERRUPTION

object Interruption_Future extends App {
  def delay(value0: => Int, ms: Int): Future[Int] =
    Future {
      val value = value0
      println(s"START $value")
      Thread.sleep(ms)
      println(s"FINISHED CALCULATING $value")
      value
    }

  // TODO:
  // Look into Fire and Forget
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

object Interruption_ZIO extends ZIOAppDefault {
  def delay(name: String, expr: => Int, ms: Int): Task[Int] =
    for {
      _      <- ZIO.debug(s"START $name")
      result <- ZIO.attempt(expr).delay(ms.millis).onInterrupt(ZIO.debug(s"INTERRUPTED $name"))
      _      <- ZIO.debug(s"END $name")
    } yield result

  private val result: UIO[(Iterable[Throwable], Iterable[Int])] =
    ZIO.partitionPar(
      List(
        delay("boom", throw new Exception("Boom!"), 500),
        delay("one", 1, 1000),
        delay("two", 2, 2000),
        delay("three", 3, 3000)
      )
    )(identity)

  val run = result.debug("RESULT")
}

object Interruption_CatsEffect extends CIOApp.Simple {

  def delay(name: String, expr: => Int, ms: Int): CIO[Int] =
    for {
      _      <- CIO.println(s"START $name")
      result <- CIO.delay(expr).delayBy(ms.millis).onCancel(CIO.println(s"INTERRUPTED $name"))
      _      <- CIO.println(s"END $name")
    } yield result

  private val result: CIO[List[Int]] = CIO
    .parSequenceN(8)(
      List(
        delay("boom", throw new Exception("Boom!"), 500),
        delay("one", 1, 1000),
        delay("two", 2, 2000),
        delay("three", 3, 3000)
      )
    )

  val run = result.attempt.flatMap(result => CIO.println(result)).void
}

// RACING

object Racing_Future extends App {
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

object Racing_ZIO extends ZIOAppDefault {
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

object Racing_CatsEffect extends CIOApp.Simple {
  def delay(name: String, expr: => Int, ms: Int): CIO[Int] =
    for {
      _      <- CIO.println(s"START $name")
      result <- CIO.delay(expr).delayBy(ms.millis).onCancel(CIO.println(s"INTERRUPTED $name"))
      _      <- CIO.println(s"END $name")
    } yield result

  val result: CIO[Either[Either[Int, Int], Int]] =
    delay("one", 1, 3000) race
      delay("two", 2, 3000) race
      delay("three", 3, 1000)

  val run = result.flatMap(result => CIO.println(s"RESULT $result")).void
}

// Stack Traces

object StackTrace_Imperative extends App {
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

object StackTrace_Future extends App {
  def methodOne(): Future[Int] =
    for {
      _      <- Future(println("Start methodOne"))
      _      <- Future(Thread.sleep(1000))
      result <- methodTwo(10)
      _      <- Future(println("Finish methodOne"))
    } yield result

  def methodTwo(int: Int): Future[Int] =
    for {
      _      <- Future(println("Start methodTwo"))
      _      <- Future(Thread.sleep(1000))
      result <- methodThree(int)
      _      <- Future(println("Finish methodTwo"))
    } yield result

  def methodThree(int: Int): Future[Int] =
    for {
      _      <- Future(println("Start methodThree"))
      _      <- Future(Thread.sleep(1000))
      result <- methodFour(int)
      _      <- Future(println("Finish methodThree"))
    } yield result

  def methodFour(i: Int): Future[Int] =
    Future(throw new Error(s"I HATE THE NUMBER $i!!!"))

  val result = methodOne()

  println(Await.result(result, scala.concurrent.duration.Duration.Inf))
}

object StackTrace_ZIO extends ZIOAppDefault {

  val methodOne: Task[Int] =
    for {
      _      <- ZIO.debug("Start methodOne")
      result <- methodTwo(10).delay(1.second)

      fib <- ZIO.debug("Finish methodOne").fork
      _   <- fib.join
      _   <- ZIO.debug("Finish methodOne")
    } yield result

  def methodTwo(int: Int): Task[Int] =
    for {
      _      <- ZIO.debug("Start methodTwo")
      result <- methodThree(int).delay(1.second)
      _      <- ZIO.debug("Finish methodTwo")
      _      <- ZIO.debug("Finish methodTwo")
    } yield result

  def methodThree(int: Int): Task[Int] =
    for {
      _      <- ZIO.debug("Start methodThree")
      result <- methodFour(int).delay(1.second)
      _      <- ZIO.debug("Finish methodThree")
      _      <- ZIO.debug("Finish methodThree")
    } yield result

  def methodFour(i: Int): Task[Int] =
    ZIO.attempt(throw new Error(s"I HATE THE NUMBER $i!!!"))

  val run = methodOne
}

object StackTrace_CatsEffect extends CIOApp.Simple {
  val methodOne: CIO[Int] =
    for {
      _      <- CIO.println("Start methodOne")
      result <- methodTwo(10).delayBy(1.second)
      _      <- CIO.println("Finish methodOne")
    } yield result

  def methodTwo(int: Int): CIO[Int] =
    for {
      _      <- CIO.println("Start methodTwo")
      result <- methodThree(int).delayBy(1.second)
      _      <- CIO.println("Finish methodTwo")
    } yield result

  def methodThree(int: Int): CIO[Int] =
    for {
      _      <- CIO.println("Start methodThree")
      result <- methodFour(int).delayBy(1.second)
      _      <- CIO.println("Finish methodThree")
    } yield result

  def methodFour(i: Int): CIO[Int] =
    CIO.delay(throw new Error(s"I HATE THE NUMBER $i!!!"))

  val run = methodOne.void
}

object Retry_Future extends App {
  def retryN[A](n: Int, expr: => A): Future[A] =
    if (n == 0) Future(expr)
    else
      Future(expr).recoverWith { _ =>
        println("RETRYING")
        retryN(n - 1, expr)
      }

  def timed[A](future: Future[A]): Future[(Duration, A)]                    = ???
  def span[A](annotation: String, future: Future[A]): Future[(Duration, A)] = ???

  def flakyFuture: Int = {
    val int = scala.util.Random.nextInt(100)
    println(s"GOT $int")
    if (int % 5 != 0) throw new Error("OOPS")
    else int
  }

  println(Await.result(retryN(3, flakyFuture), scala.concurrent.duration.Duration.Inf))
}

object Retry_ZIO extends ZIOAppDefault {
  def flakyEffect: Task[Int] =
    for {
      int <- Random.nextIntBounded(100)
      _   <- ZIO.sleep(200.millis)
      _   <- ZIO.debug(s"GOT $int")
      _   <- ZIO.fail(new Error(s"BAD NUMBER $int")).unless(int % 20 == 0)
    } yield int

  def logAround[A](name: String, task: Task[A]): Task[A] =
    for {
      _      <- ZIO.debug(s"STARTING $name")
      result <- task
      _      <- ZIO.debug(s"ENDING $name")
    } yield result

  val run = logAround("FLAKY", flakyEffect.eventually).timed.debug("WOOO")
}

object Retry_CatsEffect extends CIOApp.Simple {
  def flakyEffect: CIO[Int] =
    for {
      int <- std.Random.scalaUtilRandom[CIO].flatMap(_.nextIntBounded(100))
      _   <- CIO.println(s"GOT $int")
      _ <-
        CIO.unlessA(int % 50 == 0)(
          CIO.delay(throw new Error(s"BAD NUMBER $int"))
        )
    } yield int

  def eventually[A](cio: CIO[A]): CIO[A] =
    cio.handleErrorWith(_ => eventually(cio))

  val run = eventually(flakyEffect).flatMap(result => CIO.println(s"WOOO $result")).void
}

object Ref_ZIO extends ZIOAppDefault {

  val res: UIO[(Int, String)] =
    ZIO.succeed(10) zipPar ZIO.succeed("Hello")

  // TODO: Look into weird AtomicReference Int bug...
  def updateRef(ref: Ref[Int]): UIO[Unit] =
    ref.modify(a => (100, a + 1))

  val run =
    for {
      ref    <- Ref.make(0)
      _      <- ZIO.foreachParDiscard(1 to 100_000)(_ => updateRef(ref))
      result <- ref.get
      _      <- ZIO.debug(s"RESULT: $result")
    } yield ()
}

object Ref_CatsEffect extends CIOApp.Simple {
  def updateRef(ref: CRef[CIO, Int]): CIO[Unit] =
    ref.update(_ + 1)

  val run =
    for {
      ref    <- CRef[CIO].of(0)
      _      <- CIO.parTraverseN(8)((1 to 100_000).toList)(_ => updateRef(ref))
      result <- ref.get
      _      <- CIO.println(s"RESULT: $result")
    } yield ()
}

// Tasks:
// - implement `retry` on future
// - implement `timed` on future
// - implement `timeout` on future

// UTILS

object Utils {
  @tailrec
  def repeat[A](n: Int, start: A, f: A => A): A =
    if (n == 0) start else repeat(n - 1, f(start), f)

  implicit def javaDuration2Finite(javaDuration: java.time.Duration): FiniteDuration =
    FiniteDuration(javaDuration.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS)
}
