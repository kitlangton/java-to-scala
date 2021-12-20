package course.miscellany

import course.m7_effects.FutureUtils.debugThread

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

sealed trait FutureState[+A]

object FutureState {
  final case class Success[A](value: A)          extends FutureState[A]
  final case class Failure(exception: Throwable) extends FutureState[Nothing]
  case object Executing                          extends FutureState[Nothing]
}

class Future[A] {
  var callbacks: List[A => Unit] = Nil

  var result: FutureState[A] = FutureState.Executing

  def complete(value: A): Unit = {
    result = FutureState.Success(value)
    callbacks.foreach(f => f(value))
  }

  def foreach(f: A => Unit): Unit =
    result match {
      case FutureState.Success(a) =>
        Future.executor.execute(() => f(a))

      case FutureState.Failure(exception) =>
        ()

      case FutureState.Executing =>
        val forked = (a: A) => Future.executor.execute(() => f(a))
        callbacks = forked :: callbacks
    }

  def map[B](f: A => B): Future[B] = {
    val future = new Future[B]

    foreach { a =>
      future.complete(f(a))
    }

    future
  }

  def flatMap[B](f: A => Future[B]): Future[B] = {
    val future = new Future[B]

    foreach { a =>
      f(a).foreach(b => future.complete(b))
    }

    future
  }

  def zip[B](that: => Future[B]): Future[(A, B)] = {
    val f2 = that
    for {
      a <- this
      b <- f2
    } yield (a, b)
  }

}

object Future {

  val executor: ExecutionContextExecutor =
    ExecutionContext.global

  def apply[A](value: => A): Future[A] = {
    val future = new Future[A]

    executor.execute { () =>
      debugThread("Future.apply")
      future.complete(value)
    }

    future
  }
}

object FutureExamples extends App {

  debugThread("FutureExamples")

  def delayed[A](value: => A): Future[A] = Future {
    Thread.sleep(2000)
    value
  }

  val int    = delayed(42)
  val string = delayed("Hello")
  val bool   = delayed(true)

  (int zip string zip bool).foreach { result =>
    debugThread("future.foreach")
    println(s"Result was $result")
  }

  Thread.sleep(2000)
}
