//package course.lessons.task
//
//import scala.annotation.tailrec
//import scala.concurrent.Future
//
//object TaskExamples extends App {
//  def delay[A](millis: Long)(result: => A): Task[A] =
//    Task.async { cb =>
//      Thread.sleep(millis)
//      cb(Exit.succeed(result))
//    }
//
//  def delayFuture[A](millis: Long)(result: => A): Task[A] =
//    Task.fromFuture { ec =>
//      Future {
//        Thread.sleep(millis)
//        result
//      }(ec)
//    }
//
//  def print(any: Any) = Task(println("LOGGING: " + any))
//
//  final case class State(int: Int) {
//    def increment: State = copy(int + 1)
//  }
//
//  var int = 0
//
//  @tailrec
//  def repeat[A](n: Int, init: A)(f: A => A): A =
//    if (n == 0) init
//    else repeat(n - 1, f(init))(f)
//
//  def inc(task: Task[Int]): Task[Int] =
//    task.flatMap { n =>
//      println(s"$n thread: ${Thread.currentThread().getName}")
//      Task(n + 1)
//    }
//
//  //  repeat(1000, Task(0))(inc).runSync
//
//  def slow[A](result: => A, ms: Int): Task[A] =
//    Task.sleep(ms).as(result)
//
//  val composed =
//    slow("HELLO", 1000) race
//      slow("GOODBYE", 2000)
//        .onInterrupt(Task.debug("INTERRUPTED!"))
//
//  composed.runSync
//
//  val another = for {
//    ref    <- Ref.make(0)
//    _      <- ref.update(_ + 1).fork.repeat(999999)
//    _      <- Task(int += 1).fork.repeat(999999)
//    result <- ref.get
//    nice   <- delayFuture(1000)(1000)
//  } yield (result, int, nice)
//  //
//  //  println(another.runSync)
//
//}
