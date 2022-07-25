package course.lessons.customFuture

import course.lessons.customFuture.FutureState.{Completed, Executing}

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
//import scala.concurrent.ExecutionContext.Implicits.global

// - Future[A] represents a currently executing computation of A
// - Completed execution of A

// We have a result OR we have a list of continuations
sealed trait FutureState[A] extends Product with Serializable

object FutureState {
  // Try
  final case class Completed[A](value: A)                       extends FutureState[A]
  final case class Executing[A](continuations: List[A => Unit]) extends FutureState[A]
}

class Future[A] {
  val ref: AtomicReference[FutureState[A]] =
    new AtomicReference(Executing(List.empty))

  @tailrec
  final def complete(a: A): Unit =
    ref.get() match {
      case Completed(_) => ()
      case state @ Executing(continuations) =>
        if (ref.compareAndSet(state, Completed(a)))
          continuations.reverse.foreach(cont => cont(a))
        else
          complete(a)
    }

  // TODO: Check out double-checked lock
  @tailrec
  final def foreach(cont: A => Unit): Unit =
    ref.get() match {
      case Completed(value) =>
        cont(value)
      case state @ Executing(continuations) =>
        if (!ref.compareAndSet(state, Executing(cont :: continuations)))
          foreach(cont)
    }

  def map[B](f: A => B): Future[B] = {
    val futureB = new Future[B]
    this.foreach(a => futureB.complete(f(a)))
    futureB
  }

  def flatMap[B](f: A => Future[B]): Future[B] = {
    val futureB = new Future[B]
    this.foreach { a =>
      f(a).foreach(b => futureB.complete(b))
    }
    futureB
  }

}

object Future {

  val myRunnable: Runnable = () => println("HEY")

  def apply[A](expr: => A)(implicit ec: ExecutionContext): Future[A] = {
    val future = new Future[A]
    ec.execute { () =>
      println(s"THREAD NAME ${Thread.currentThread().getName}")
      future.complete(expr)
    }
    future
  }
}
