//package course.lessons.task
//
//import java.util.concurrent.atomic.AtomicReference
//
//object Ref {
//  def make[A](value: A): Task[Ref[A]] = Task {
//    new Ref(new AtomicReference[A](value))
//  }
//}
//
//class Ref[A](value: AtomicReference[A]) {
//  def get: Task[A] = Task {
//    value.get()
//  }
//
//  def set(a: A): Task[Unit] = Task {
//    value.set(a)
//  }
//
//  private def unsafeModify[B](f: A => (B, A)): B = {
//    var loop = true
//    var b: B = null.asInstanceOf[B]
//    while (loop) {
//      val current = value.get
//      val tuple   = f(current)
//      b = tuple._1
//      loop = !value.compareAndSet(current, tuple._2)
//    }
//    b
//  }
//
//  def modify[B](f: A => (B, A)): Task[B] =
//    Task {
//      unsafeModify(f)
//    }
//
//  def update(f: A => A): Task[Unit] = Task {
//    value.updateAndGet((t: A) => f(t))
//  }
//
//  def updateAndGet(f: A => A): Task[A] = Task {
//    value.updateAndGet((t: A) => f(t))
//  }
//
//}
