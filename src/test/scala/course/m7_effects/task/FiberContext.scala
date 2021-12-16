package course.m7_effects.task

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.Try
import scala.util.control.NonFatal

sealed trait FiberState[A] extends Product with Serializable

object FiberState {
  def empty[A]: FiberState[A] = Executing(Nil)

  final case class Completed[A](value: A)                   extends FiberState[A]
  final case class Executing[A](callbacks: List[A => Unit]) extends FiberState[A]
}

trait Fiber[+A] {
  def join: Task[A]
}

class FiberContext[A](task: Task[A]) extends Fiber[A] {

  type ErasedTask = Task[Any]
  type Cont       = Any => ErasedTask
  val stack: mutable.Stack[Cont] =
    scala.collection.mutable.Stack.empty[Cont]

  var currTask: ErasedTask =
    task.asInstanceOf[ErasedTask]

  def continue(result: Any): Unit =
    if (stack.isEmpty) {
      complete(Exit.Success(result.asInstanceOf[A]))
      currTask = null
    } else {
      val cont = stack.pop()
      currTask = cont(result)
    }

  def fail(throwable: Throwable): Unit = {
    complete(Exit.Failure(throwable))
    currTask = null
  }

  def handleFailure(error: Throwable): Unit = {
    @tailrec
    def loop(cont: Cont): Unit =
      if (cont eq null) {
        fail(error)
      } else if (cont.isInstanceOf[Task.Fold[_, _]]) {
        currTask = cont.asInstanceOf[Task.Fold[Any, Any]].onFailure(error)
      } else {
        loop(Try(stack.pop()).getOrElse(null))
      }

    loop(Try(stack.pop()).getOrElse(null))
  }

  def run(): Unit =
    ExecutionContext.global.execute { () =>
      try while (currTask ne null)
        currTask match {
          case Task.Succeed(value) =>
            continue(value())

          case Task.FlatMap(task, continuation) =>
            stack.push(continuation.asInstanceOf[Cont])
            currTask = task

          case Task.Fail(error) =>
            handleFailure(error)

          case Task.Async(register) =>
            currTask = null
            register {
              case Exit.Success(value) =>
                continue(value)
                run()
              case Exit.Failure(exception) =>
                throw new RuntimeException(exception)
            }

          case fold @ Task.Fold(task, _, k) =>
            stack.push(fold.asInstanceOf[Cont])
            currTask = task
        } catch {
        case NonFatal(e) =>
          handleFailure(e)
          if (currTask ne null) run()
      }
    }

  val result: AtomicReference[FiberState[Exit[A]]] =
    new AtomicReference(FiberState.empty[Exit[A]])

  def complete(value: Exit[A]): Unit = {
    val current = result.get
    current match {
      case FiberState.Completed(_) => ()

      case FiberState.Executing(callbacks) =>
        if (result.compareAndSet(current, FiberState.Completed(value))) {
          callbacks.foreach { cb =>
            cb(value)
          }
        } else
          complete(value)
    }
  }

  @tailrec
  final def addCallback(cb: Exit[A] => Unit): Unit = {
    val current = result.get
    current match {
      case FiberState.Completed(value) =>
        cb(value)
      case FiberState.Executing(callbacks) =>
        if (!result.compareAndSet(current, FiberState.Executing(cb :: callbacks))) addCallback(cb)
    }
  }

  override def join: Task[A] =
    Task.async { cb =>
      addCallback(cb)
    }

}
