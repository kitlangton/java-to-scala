package course.archive.cohort1.m7_effects.task

import java.util.concurrent.ScheduledThreadPoolExecutor
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.function.UnaryOperator
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

sealed trait Exit[+A]

object Exit {
  def succeed[A](value: A): Exit[A]     = Exit.Success(value)
  def fail(cause: Cause): Exit[Nothing] = Exit.Failure(cause)

  final case class Success[A](value: A)  extends Exit[A]
  final case class Failure(cause: Cause) extends Exit[Nothing]
}

sealed trait Task[+A] { self =>

  def tap(f: A => Task[Any]): Task[A] =
    self.flatMap(a => f(a).as(a))

  def repeat(n: Int): Task[A] =
    if (n > 0) self *> repeat(n - 1)
    else self

  def *>[B](that: => Task[B]): Task[B] =
    flatMap(_ => that)

  def <*(that: Task[Unit]): Task[A] =
    self.zip(that).map(_._1)

  def delay(millis: Int) =
    Task.sleep(millis) *> self

  def fold[B](onFail: Throwable => Task[B], onSucceed: A => Task[B]): Task[B] =
    Task.Fold[A, B](
      self,
      {
        case Cause.Die(throwable) => onFail(throwable)
        case Cause.Interrupt      => Task.Fail(Cause.Interrupt)
      },
      onSucceed
    )

  def foldCause[B](onFail: Cause => Task[B], onSucceed: A => Task[B]): Task[B] =
    Task.Fold[A, B](self, onFail, onSucceed)

  def orElseSucceed[A1 >: A](value: => A1): Task[A1] =
    foldCause(
      _ => Task.succeed(value),
      _ => self
    )

  def either: Task[Either[Throwable, A]] =
    fold(
      ex => Task.succeed(Left(ex)),
      a => Task.succeed(Right(a))
    )

  def ensuring(finalizer: Task[Any]): Task[A] =
    foldCause(
      cause => finalizer *> Task.Fail(cause),
      a => finalizer *> Task.succeed(a)
    )

  def debug: Task[A] =
    tap(a => Task.debug(s"DEBUG: $a"))

  def debug(prefix: String): Task[A] =
    tap(a => Task.debug(s"$prefix: $a"))

  def timed: Task[(A, Long)] =
    for {
      start <- Task.currentTime
      a     <- self
      end   <- Task.currentTime
    } yield (a, end - start)

  def onInterrupt(task: Task[Any]): Task[A] =
    foldCause(
      {
        case Cause.Die(throwable) => Task.Fail(Cause.Die(throwable))
        case Cause.Interrupt      => task *> Task.Fail(Cause.Interrupt)
      },
      a => Task(a)
    )

  def fork: Task[Fiber[A]] =
    Task.Fork(self)

  def runSync: Exit[A] = {
    val countdownLatch = new java.util.concurrent.CountDownLatch(1)

    val context = new FiberContext[A](self)

    context.await { _ =>
      countdownLatch.countDown()
    }

    countdownLatch.await()

    context.result.get() match {
      case FiberState.Completed(exit) => exit
      case result                     => throw new Error(s"OH NO $result")
    }
  }

  import Task._

  def map[B](f: A => B): Task[B] =
    flatMap(a => Task.succeed(f(a)))

  def as[B](value: => B): Task[B] =
    map(_ => value)

  def flatMap[B](f: A => Task[B]): Task[B] =
    FlatMap(self, f)

  def raceEither[B](that: Task[B]): Task[Either[A, B]] = {
    val called                       = new AtomicBoolean(false)
    var result: Either[A, B] => Unit = null

    val go = Task.async[Either[A, B]] { cb =>
      result = (a: Either[A, B]) =>
        if (called.compareAndSet(false, true)) {
          println(s"CALLING $a")
          cb(Exit.succeed(a))
        }
    }

    val raced = for {
      fa <- self.tap(a => Task(result(Left(a)))).fork
      fb <- that.tap(b => Task(result(Right(b)))).fork
      _ <-
        (fa.join.tap(_ => fb.interrupt) zipPar
          fb.join.tap(_ => fa.interrupt))
          .foldCause(_ => Task(()), _ => Task(()))
    } yield ()

    (raced zipPar go).map(_._2)
  }

  def race[A1 >: A](that: Task[A1]): Task[A1] =
    raceEither(that).map(_.merge)

  def zip[B](that: Task[B]): Task[(A, B)] =
    for {
      a <- self
      b <- that
    } yield (a, b)

  def zipPar[B](that: Task[B]): Task[(A, B)] =
    for {
      fiber <- self.fork
      b     <- that
      a     <- fiber.join
    } yield (a, b)
}

object Task {

  val currentTime: Task[Long] =
    Task(System.currentTimeMillis())

  def debug(value: Any): Task[Unit] =
    Task(
      println(scala.Console.BLUE + value + scala.Console.RESET)
    )

  def fromFuture[A](create: ExecutionContext => Future[A]): Task[A] = {
    implicit val ec = ExecutionContext.global
    val future      = create(ec)
    Task.async { cb =>
      future.onComplete {
        case Failure(exception) => cb(Exit.fail(Cause.Die(exception)))
        case Success(value)     => cb(Exit.succeed(value))
      }
    }
  }

  def sleep(millis: Int): Task[Unit] =
    Task.async[Unit] { cb =>
      Task.scheduler.schedule(
        new Runnable {
          def run(): Unit = cb(Exit.succeed(()))
        },
        millis,
        java.util.concurrent.TimeUnit.MILLISECONDS
      )
    }

  def fail(error: Throwable): Task[Nothing] =
    Task.Fail(Cause.Die(error))

  def apply[A](a: => A): Task[A] =
    Effect(() => a)

  def succeed[A](a: => A): Task[A] =
    Effect(() => a)

  def async[A](register: (Exit[A] => Unit) => Unit): Task[A] =
    Async(register)

  def foreach[A, B](as: List[A])(f: A => Task[B]): Task[List[B]] =
    as.foldRight(Task.succeed(Nil): Task[List[B]]) { (a, acc) =>
      f(a).zip(acc).map { case (b, bs) => b :: bs }
    }

  def foreachPar[A, B](as: List[A])(f: A => Task[B]): Task[List[B]] =
    as.foldRight(Task.succeed(Nil): Task[List[B]]) { (a, acc) =>
      f(a).zipPar(acc).map { case (b, bs) => b :: bs }
    }

  final case class Effect[A](value: () => A) extends Task[A]

  final case class FlatMap[A, B](task: Task[A], continuation: A => Task[B]) extends Task[B]

  final case class Async[A](register: (Exit[A] => Unit) => Unit) extends Task[A]

  final case class Fail(cause: Cause) extends Task[Nothing]

  final case class Fold[A, B](task: Task[A], onFailure: Cause => Task[B], onSuccess: A => Task[B])
      extends Task[B]
      with (A => Task[B]) {
    def apply(a: A): Task[B] = onSuccess(a)
  }

  final case class Fork[A](self: Task[A]) extends Task[Fiber[A]]

  private val scheduler: ScheduledThreadPoolExecutor = {
    val service = new ScheduledThreadPoolExecutor(1)
    service.setRemoveOnCancelPolicy(true)
    service
  }
}

class Ref[A](value: AtomicReference[A]) {
  def get: Task[A] = Task {
    value.get()
  }

  def set(a: A): Task[Unit] = Task {
    value.set(a)
  }

  private def unsafeModify[B](f: A => (B, A)): B = {
    var loop = true
    var b: B = null.asInstanceOf[B]
    while (loop) {
      val current = value.get
      val tuple   = f(current)
      b = tuple._1
      loop = !value.compareAndSet(current, tuple._2)
    }
    b
  }

  def modify[B](f: A => (B, A)): Task[B] =
    Task {
      unsafeModify(f)
    }

  def update(f: A => A): Task[Unit] = Task {
    value.updateAndGet((t: A) => f(t))
  }

  def updateAndGet(f: A => A): Task[A] = Task {
    value.updateAndGet((t: A) => f(t))
  }

}

object Ref {
  def make[A](value: A): Task[Ref[A]] = Task {
    new Ref(new AtomicReference[A](value))
  }
}

object TaskExamples extends App {
  def delay[A](millis: Long)(result: => A): Task[A] =
    Task.async { cb =>
      Thread.sleep(millis)
      cb(Exit.succeed(result))
    }

  def delayFuture[A](millis: Long)(result: => A): Task[A] =
    Task.fromFuture { ec =>
      Future {
        Thread.sleep(millis)
        result
      }(ec)
    }

  def print(any: Any) = Task(println("LOGGING: " + any))

  final case class State(int: Int) {
    def increment: State = copy(int + 1)
  }

  var int = 0

  val another = for {
    ref    <- Ref.make(0)
    _      <- ref.update(_ + 1).fork.repeat(999999)
    _      <- Task(int += 1).fork.repeat(999999)
    result <- ref.get
    nice   <- delayFuture(1000)(1000)
  } yield (result, int, nice)

  println(another.runSync)

}
