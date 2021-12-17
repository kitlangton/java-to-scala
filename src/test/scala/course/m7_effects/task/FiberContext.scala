package course.m7_effects.task

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.Try
import scala.util.control.NonFatal

sealed trait Cause

object Cause {
  final case class Die(throwable: Throwable) extends Cause
  case object Interrupt                      extends Cause
}

sealed trait FiberState[A] extends Product with Serializable

object FiberState {
  def empty[A]: FiberState[A] = Executing(Nil)

  final case class Completed[A](value: Exit[A])                  extends FiberState[A]
  final case class Executing[A](callbacks: List[Exit[A] => Any]) extends FiberState[A]
}

trait Fiber[+A] {
  def interrupt: Task[Unit]

  def join: Task[A]
}

class FiberContext[A](task: Task[A]) extends Fiber[A] {
  type ErasedTask = Task[Any]
  type Cont       = Any => ErasedTask

  val isInterrupted   = new AtomicBoolean(false)
  val isInterrupting  = new AtomicBoolean(false)
  val isInterruptible = new AtomicBoolean(true)

  def shouldInterrupt(): Boolean =
    isInterrupted.get() && isInterruptible.get() && !isInterrupting.get()

  val result: AtomicReference[FiberState[A]] =
    new AtomicReference(FiberState.empty[A])

  val stack: mutable.Stack[Cont] =
    scala.collection.mutable.Stack.empty[Cont]

  def nextTask(result: Any): ErasedTask =
    if (stack.isEmpty) {
      complete(Exit.Success(result.asInstanceOf[A]))
      null
    } else {
      val cont = stack.pop()
      cont(result)
    }

  def fail(cause: Cause): Any =
    complete(Exit.Failure(cause))

  def findNextErrorHandler(cause: Cause): ErasedTask = {
    @tailrec
    def loop(): ErasedTask =
      Try(stack.pop()).getOrElse(null) match {
        case null =>
          fail(cause)
          null
        case cont: Task.Fold[_, _] =>
          cont.onFailure(cause)
        case _ =>
          loop()
      }

    loop()
  }

  val isSuspended = new AtomicBoolean(false)

  def run(task: ErasedTask): Unit = {
    isSuspended.set(false)
    var currTask: ErasedTask = task
    ExecutionContext.global.execute { () =>
      try while (currTask ne null)
        if (shouldInterrupt()) {
          isInterrupting.set(true)
          stack.push(_ => currTask)
          currTask = Task.Fail(Cause.Interrupt)
        } else
          currTask match {
            case Task.Effect(value) =>
              val result = value()
              currTask = nextTask(result)

            case Task.FlatMap(task, continuation) =>
              stack.push(continuation.asInstanceOf[Cont])
              currTask = task

            case Task.Fail(error) =>
              currTask = findNextErrorHandler(error)

            case Task.Async(register) =>
              currTask = null

              register {
                case Exit.Success(value) =>
                  if (isSuspended.get()) {
                    run(nextTask(value))
                  } else {
                    currTask = nextTask(value)
                  }

                case Exit.Failure(cause) =>
                  run(Task.Fail(cause))
              }
              if (currTask == null)
                isSuspended.set(true)

            case fold @ Task.Fold(task, _, _) =>
              stack.push(fold.asInstanceOf[Cont])
              currTask = task

            case Task.Fork(task) =>
              currTask = Task.succeed(new FiberContext[Any](task))

          } catch {
        case NonFatal(e) =>
          currTask = findNextErrorHandler(Cause.Die(e))
//          run(task)
      }
    }
  }

  def complete(value: Exit[A]): Unit =
    result.get match {
      case FiberState.Completed(_) =>
        throw new Error("Fiber already completed")

      case oldState @ FiberState.Executing(callbacks) =>
        if (result.compareAndSet(oldState, FiberState.Completed(value))) {
          callbacks.foreach(cb => cb(value))
        } else
          complete(value)
    }

  @tailrec
  final def await(cb: Exit[A] => Unit): Unit =
    result.get match {
      case FiberState.Completed(value) =>
        cb(value)
      case oldState @ FiberState.Executing(callbacks) =>
        if (!result.compareAndSet(oldState, FiberState.Executing(cb :: callbacks))) await(cb)
    }

  override def join: Task[A] =
    Task.async(await)

  override def interrupt: Task[Unit] =
    Task {
      isInterrupted.set(true)
      if (isSuspended.get()) {
        run(Task.Fail(Cause.Interrupt))
      }
    }

  run(task)
}
