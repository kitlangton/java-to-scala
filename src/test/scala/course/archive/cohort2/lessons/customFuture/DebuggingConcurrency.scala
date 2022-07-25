package course.lessons.customFuture

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.concurrent.ExecutionContext

final case class MyInt(int: Int) {
  def increment(): MyInt = MyInt(int + 1)
}

class NonAtomicInteger(init: Int) {
  private val value: AtomicReference[MyInt] =
    new AtomicReference(MyInt(init))

  def get(): MyInt = value.get()

  def increment(): Unit = {
    val oldValue = value.get
    val newValue = oldValue.increment()
    if (!value.compareAndSet(oldValue, newValue)) increment()
  }
}

object DebuggingConcurrency extends App {
  val nonAtomic = new AtomicInteger(0)

  def forkIncrement(): Unit =
    ExecutionContext.global.execute(() => nonAtomic.getAndIncrement())

  (1 to 10_000).foreach(_ => forkIncrement())

  Thread.sleep(200)

  println(nonAtomic.get())
}
