package course.archive.cohort1.m7_effects

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object FutureUtils {

  /** A function that takes a value and returns a Future[A] that will be
    * completed with the value after a the given delay in milliseconds.
    */
  def delay[A](millis: Int)(result: => A): Future[A] =
    Future {
      Thread.sleep(millis)
      result
    }

  def debugThread(message: String): Unit = {
    val thread = Thread.currentThread
    println(s"Thread ($message): ${thread.getName}")
  }
}
