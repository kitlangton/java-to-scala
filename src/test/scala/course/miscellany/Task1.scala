package course.miscellany

import scala.concurrent.ExecutionContext

// Continuation Passing
// Callbacks
// A => Unit

trait Fiber[+A] {
  def join: Task[A]
}

class FiberImpl[A]() extends Fiber[A] {

  var result    = Option.empty[A]
  var callbacks = List.empty[A => Any]

  def start(task: Task[A]): Unit =
    ExecutionContext.global.execute { () =>
      task.runAsync { a =>
        result = Some(a)
        callbacks.foreach(cb => cb(a))
      }
    }

  def join: Task[A] =
    Task.async[A] { cb =>
      result match {
        case Some(a) => cb(a)
        case None =>
          callbacks = cb :: callbacks
      }
    }

}

sealed trait Task[+A] {
  def runAsync(callback: A => Any): Unit

  def *>[B](that: => Task[B]): Task[B] =
    zip(that).map(_._2)

  def zip[B](that: => Task[B]): Task[(A, B)] =
    flatMap(a => that.map(b => (a, b)))

  def map[B](f: A => B): Task[B] =
    flatMap(a => Task(f(a)))

  def fork: Task[Fiber[A]] =
    Task.Fork(this)

  def flatMap[B](f: A => Task[B]): Task[B] =
    Task.FlatMap(this, f)
}

object Task {
  def debug(msg: => Any): Task[Unit] =
    Task(println(msg))

  def apply[A](a: => A): Task[A] =
    Task.Effect(() => a)

  type Callback[A] =
    A => Any

  def async[A](f: Callback[A] => Any): Task[A] =
    Task.Async[A](f)

  case class Effect[A](a: () => A) extends Task[A] {
    override def runAsync(callback: A => Any): Unit =
      callback(a())
  }

  final case class FlatMap[A, B](task: Task[A], f: A => Task[B]) extends Task[B] {
    override def runAsync(callback: B => Any): Unit =
      task.runAsync { a =>
        f(a).runAsync { b =>
          callback(b)
        }
      }
  }

  final case class Async[A](f: (A => Any) => Any) extends Task[A] {
    override def runAsync(callback: A => Any): Unit =
      f(callback)
  }

  case class Fork[A](task: Task[A]) extends Task[Fiber[A]] {
    override def runAsync(callback: Fiber[A] => Any): Unit = {
      val fiber = new FiberImpl[A]
      fiber.start(task)
      callback(fiber)
    }
  }
}

object TaskExamples extends App {

  val longRunningTask =
    Task.async[Int] { cb =>
      println("START")
      Thread.sleep(2000)
      println("END")
      cb(100)
    }

  val composed =
    for {
      fiber <- longRunningTask.fork
      y     <- longRunningTask
      x     <- fiber.join
    } yield (x, y)

  composed.runAsync(int => println(s"YAY! $int"))
}
