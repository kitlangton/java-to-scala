package course.archive.cohort3

import java.time.{Duration, Instant}
import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}
import scala.concurrent.ExecutionContext

// CPS
// sync: () => A
// async: (A => Unit) => Unit
// useA: (A) => Int
// val a = sync()
// async { a => useA(a) }
// val a = async()

// () => A
// (A => ?) => ?

final case class Effect[A](run: (A => Unit) => Unit) { self =>

  def fork: Effect[Fiber[A]] =
    Effect[Fiber[A]] { cont =>
      val fiberA: Fiber[A] = new Fiber[A]
      ExecutionContext.global.execute(() => run(a => fiberA.complete(a)))
      cont(fiberA)
    }

  def debug: Effect[A] =
    for {
      a <- self
      _ <- Effect.printLine(s"${Thread.currentThread().getName}: $a")
    } yield a

  def map[B](f: A => B): Effect[B] =
    Effect[B] { cont =>
      run(a => cont(f(a)))
    }

  def as[B](value: => B): Effect[B] =
    map(_ => value)

  def zip[B](that: => Effect[B]): Effect[(A, B)] =
    for {
      a <- self
      b <- that
    } yield (a, b)

  def zipPar[B](that: => Effect[B]): Effect[(A, B)] =
    for {
      fa <- self.fork
      fb <- that.fork
      a  <- fa.join
      b  <- fb.join
    } yield (a, b)

  def flatMap[B](f: A => Effect[B]): Effect[B] =
    Effect[B] { contB =>
      run { a =>
        f(a).run { b =>
          contB(b)
        }
      }
    }

  def timed: Effect[(Duration, A)] =
    for {
      start    <- Effect.succeed(Instant.now)
      a        <- self
      duration <- Effect.succeed(Duration.between(start, Instant.now))
    } yield (duration, a)
}

object Effect extends App {

  def succeed[A](value: => A): Effect[A] =
    Effect(cont => cont(value))

  private val scheduler: ScheduledThreadPoolExecutor =
    new ScheduledThreadPoolExecutor(8)

  def sleep(ms: Int): Effect[Unit] =
    Effect[Unit] { cont =>
      val runnable: Runnable = () => cont()
      scheduler.schedule(runnable, ms, TimeUnit.MILLISECONDS)
    }

//  succeed(10).run(int => println(s"INT IS $int"))
//  Effect[Int](cont => cont(10)).run(int => println(s"INT IS $int"))
//  ((cont: Int => Unit) => cont(10))(int => println(s"INT IS $int"))
//  ((int: Int) => println(s"INT IS $int"))(10)
//  println(s"INT IS ${10}")

  def delayed[A](value: => A): Effect[A] =
    Effect.sleep(1000).as(value)

  def printLine(message: String): Effect[Unit] = Effect.succeed {
    println(s"${Thread.currentThread().getName}: $message")
  }

  val readName: Effect[String] = Effect.succeed {
    scala.io.StdIn.readLine()
  }

  //   pure: Blueprint To Generate A Random Number
  // impure: Actually Generating A Random Number

  private val cool = delayed("Hello").debug
  // time 0 1 2 3 4 5 6
  // v1 ------->"hello" ------->"World" ------->"How are you"
  //
  // T1 fa fb fc    abc
  // T2:  ------->"hello"
  // T3:  ------->"World"
  // T4:  ------->"How are you"
  //
  //
  val composed: Effect[String] =
    (cool zipPar
      delayed("world").debug zipPar
      delayed("How are you").debug).map { case ((a, b), c) =>
      s"$a $b $c"
    }

//  composed.timed.debug.run { _ => }
}
