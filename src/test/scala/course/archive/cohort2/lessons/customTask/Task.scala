package course.lessons.customTask

import java.time.Instant
import java.util.concurrent.ScheduledThreadPoolExecutor
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.global

trait Fiber[+A] {
  def join: Task[A]
}

sealed trait FiberState[A] extends Product with Serializable

object FiberState {
  final case class Executing[A](continuations: List[A => Unit]) extends FiberState[A]
  final case class Completed[A](value: A)                       extends FiberState[A]
}

class FiberImpl[A] extends Fiber[A] {
  val stateRef: AtomicReference[FiberState[A]] = new AtomicReference[FiberState[A]](FiberState.Executing(List.empty))

  @tailrec
  final def complete(value: A): Unit =
    stateRef.get() match {
      case executing @ FiberState.Executing(continuations) =>
        if (stateRef.compareAndSet(executing, FiberState.Completed(value)))
          continuations.foreach(cont => cont(value))
        else
          complete(value)

      case FiberState.Completed(value) =>
        throw new IllegalStateException(s"Fiber already completed with value: $value")
    }

  @tailrec
  final def await(cont: A => Unit): Unit =
    stateRef.get() match {
      case executing @ FiberState.Executing(continuations) =>
        if (!stateRef.compareAndSet(executing, FiberState.Executing(cont :: continuations)))
          await(cont)
      case FiberState.Completed(value) =>
        cont(value)
    }

  override def join: Task[A] =
    new Task[A] {
      override def run(cont: A => Unit): Unit =
        await(cont)
    }
}

trait Task[+A] { self =>

  // TODO:
  // - make async
  // - model failures
  // - CPS
//  def run: A
  def run(cont: A => Unit): Unit

  def map[B](f: A => B): Task[B] =
    self.flatMap { a =>
      Task.succeed(f(a))
    }

  def flatMap[B](f: A => Task[B]): Task[B] =
    new Task[B] {
      override def run(cont: B => Unit): Unit =
        self.run { a =>
          f(a).run { b =>
            cont(b)
          }
        }
    }

  def repeatN(n: Int): Task[A] =
    if (n <= 1) self
    else self zipRight repeatN(n - 1)

  def zip[B](that: => Task[B]): Task[(A, B)] =
    for {
      a <- self
      b <- that
    } yield (a, b)

  def zipPar[B](that: => Task[B]): Task[(A, B)] =
    for {
      fiberA <- self.fork
      b      <- that
      a      <- fiberA.join
    } yield (a, b)

  def zipRight[B](that: => Task[B]): Task[B] =
    for {
      _ <- self
      b <- that
    } yield b

  def timed: Task[(Long, A)] =
    for {
      start <- Task.succeed(Instant.now())
      a     <- self
      end   <- Task.succeed(Instant.now())
    } yield (end.toEpochMilli - start.toEpochMilli, a)

  def debug(message: String): Task[A] =
    for {
      result <- self
      _      <- Task.printLine(s"$message: $result")
    } yield result

  def fork: Task[Fiber[A]] = Task.succeed {
    val fiber = new FiberImpl[A]
    global.execute { () =>
      self.run { a =>
        fiber.complete(a)
      }
    }
    fiber
  }

}

object Task extends TaskApp {

  def succeed[A](value: => A): Task[A] =
    new Task[A] {
      override def run(cont: A => Unit): Unit =
        cont(value)
    }

  private val scheduler: ScheduledThreadPoolExecutor =
    new ScheduledThreadPoolExecutor(8)

  def sleep(millis: Int): Task[Unit] =
    new Task[Unit] {
      override def run(cont: Unit => Unit): Unit = {
        val runnable: Runnable = () => cont(())
        scheduler.schedule(runnable, millis, java.util.concurrent.TimeUnit.MILLISECONDS)
      }
    }

  def printLine(message: String): Task[Unit] =
    Task.succeed(println(message))

  val printMyName: Task[Unit] =
    printLine("Kit")

  // flatMap == sequencing

  val stringTask: Task[String] =
    for {
      _ <- Task.sleep(1000)
    } yield "Hello!!!"

  def delay[A](expr: => A, ms: Int): Task[A] =
    for {
      a <- Task.succeed(expr)
      _ <- Task.printLine(s"STARTING $a")
      _ <- Task.sleep(ms)
      _ <- Task.printLine(s"FINISHED $a")
    } yield a

  val intTask: Task[Int] = stringTask.map(_.length)

  // Task[A] => Task[Fiber[A]]
  // Fiber[A]

  // A --------> B ---------> (A + B)

  // A.fork -> B ------> A -> (A + B)
  //      -----------> join

  // - Fiber[A]
  // - Task#fork
  // - Fiber#join

  val composedBAD =
    for {
      a <- delay("HELLO", 2000)
      b <- delay("WORLD", 2000)
    } yield a + b

  val composed =
    for {
      fiberA <- delay("HELLO", 2000).fork
      b      <- delay("WORLD", 2000)
      a      <- fiberA.join
    } yield a + b

  val zippedPar =
    (delay("HELLO", 2000) zipPar delay("WORLD", 2000))
      .map { case (str, str1) => str + str1 }

  val run = zippedPar.timed.debug("COOL")
}

trait TaskApp {
  def run: Task[Any]

  def main(args: Array[String]): Unit =
    run.run(_ => ())
}
