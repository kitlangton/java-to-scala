package course.m7_effects.task

sealed trait Exit[+A]

object Exit {
  def succeed[A](value: A): Exit[A]         = Exit.Success(value)
  def fail(error: Throwable): Exit[Nothing] = Exit.Failure(error)

  final case class Success[A](value: A)          extends Exit[A]
  final case class Failure(exception: Throwable) extends Exit[Nothing]
}

sealed trait Task[+A] { self =>
  def tap(f: A => Task[Any]): Task[A] =
    self.flatMap(a => f(a).as(a))

  def repeat(n: Int): Task[A] =
    if (n > 0) self *> repeat(n - 1)
    else self

  def *>[B](that: => Task[B]): Task[B] =
    flatMap(_ => that)

  def fold[B](onFail: Throwable => Task[B], onSucceed: A => Task[B]): Task[B] =
    Task.Fold[A, B](self, onFail, onSucceed)

  def orElseSucceed[A1 >: A](value: => A1): Task[A1] =
    fold(
      _ => Task.succeed(value),
      _ => self
    )

  def runSync: Exit[A] = {
    val countdownLatch = new java.util.concurrent.CountDownLatch(1)

    val context = new FiberContext[A](self)

    context.addCallback { _ =>
      countdownLatch.countDown()
    }

    context.run()

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

  def zip[B](that: Task[B]): Task[(A, B)] =
    for {
      a <- self
      b <- that
    } yield (a, b)
}

object Task {
  def fail(error: Throwable): Task[Nothing] =
    Task.Fail(error)

  def succeed[A](a: => A): Task[A] =
    Succeed(() => a)

  def async[A](register: (Exit[A] => Unit) => Unit): Task[A] =
    Async(register)

  final case class Succeed[A](value: () => A) extends Task[A]

  final case class FlatMap[A, B](task: Task[A], continuation: A => Task[B]) extends Task[B]

  final case class Async[A](register: (Exit[A] => Unit) => Unit) extends Task[A]

  final case class Fail(error: Throwable) extends Task[Nothing]

  final case class Fold[A, B](task: Task[A], onFailure: Throwable => Task[B], onSuccess: A => Task[B])
      extends Task[B]
      with (A => Task[B]) {
    def apply(a: A): Task[B] = onSuccess(a)
  }
}

object TaskExamples extends App {
  def delay[A](millis: Long)(result: A): Task[A] =
    Task.async { cb =>
      Thread.sleep(millis)
      cb(Exit.succeed(result))
    }

  val complex =
    for {
      a <- Task.succeed(1)
      b <- Task.succeed(2)
      c <- Task.succeed(3)
      _ <- Task.fail(new Exception("boom"))
      d <- delay(1000)(100)
    } yield a + b + c + d

  val program =
    complex
      .orElseSucceed(999)
      .tap(result => Task.succeed(println(s"RESULT $result")))
      .repeat(10)

  println(program.runSync)
}
