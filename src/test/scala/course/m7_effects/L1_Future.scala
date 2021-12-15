package course.m7_effects

import course.Lesson
import zio.test._

import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import FutureUtils._
import zio.test.TestAspect.{ignore, nonFlaky}

object LazyVal extends App {

  val name = {
    println("INITIALIZING `name`")
    "George Wilkerson"
  }

  println(name)
}

object ByNameParameters extends App {

  // What is wrong with this?
  def ifElse(cond: Boolean, onTrue: Any, onFalse: Any): Unit =
    if (true) onTrue else onFalse

  ifElse(
    cond = true,
    onTrue = println("IT'S TRUE"),
    onFalse = println("IT'S FALSE")
  )

}

object FutureExample1 extends App {

  /** Step 1 - Create a Future
    *
    * The body of `Future.apply` is executed asynchronously in a different
    * thread. This future will complete with the result of the body, when the
    * body completes.
    */
  val future: Future[String] = Future {
    println("Starting future!")
    Thread.sleep(1000)
    println("Completing future!")
    "Hello"
  }

  /** We must wait for the future to complete, otherwise the program will exit.
    * This is because the app will exit when the main thread finishes.
    */
  Await.result(future, Duration.Inf)
}

object FutureComposition extends App {

  def downloadFile(name: String): Future[String] = Future {
    println(s"Downloading file $name...")
    Thread.sleep(1000)
    println(s"Downloaded file $name!")
    """
Here is the first line of the file.
Here is the second line of the file.
Here is the third line of the file.
Here is the fourth line of the file.
Here is the fifth line of the file.
      """.trim
  }

  final case class Config(linesRequired: Int)

  def getConfig(name: String): Future[Config] = Future {
    println(s"\nGetting config for $name...")
    Thread.sleep(1000)
    println(s"Got config for $name!")
    Config(linesRequired = 2)
  }

  def processFile(string: String): Future[List[String]] = Future {
    println("\nProcessing file...")
    Thread.sleep(1000)
    println("Processed file!")
    string.split("\n").map(_.toUpperCase).toList
  }

  // - Referential Transparency
  val composed: Future[List[String]] = for {
    file          <- downloadFile("file.txt")
    config        <- getConfig("file.txt")
    processedFile <- processFile(file)
  } yield processedFile.take(config.linesRequired)

  composed.foreach { result =>
    println(s"\nResult: $result")
  }

  Await.result(composed, Duration.Inf)
}

object FutureFailures extends App {
  val computation: Future[Int] =
    Future {
      println("Starting computation!")
      Thread.sleep(1000)
      println("Completing computation!")
      val zero = 0
      1 / zero
    }
//    .recover { case e: ArithmeticException =>
//      println(s"Computation failed with '$e'")
//      -999
//    }

  val mapped =
    computation
      .foreach { result =>
        println(s"\nResult: $result")
      }

  Await.result(computation, Duration.Inf)
//  Thread.sleep(2000)
}

object WebRequests extends App {

  // Make an HTTP Get Request in Java Synchronously
  def getUrl(url: String): String = {
    val source = scala.io.Source.fromURL(url)
    val lines  = source.mkString
    source.close()
    lines
  }

  // future version
  def getUrlFuture(url: String): Future[String] = Future {
    getUrl(url)
  }

  val cool = getUrl("https://www.wikipedia.com")

  println(cool)

}

object L1_Future extends Lesson {

  /** ✏ EXERCISE
    *
    * Use `Future.successful` to create a future that completes with the string
    * "Howdy!".
    */
  val testSuccessful = suite("successful")(
    testM("successful") {
      lazy val future: Future[String] = ???

      testFuture(future) { result =>
        assertTrue(result.right.get == "Howdy!")
      }
    }
  ) @@ ignore

  /** ✏ EXERCISE
    *
    * Use `Future.failed` to create a future that fails with a `BadTime` error
    * with the message "Oh no!".
    */
  final case class BadTime(message: String) extends Throwable

  val testFailed = suite("failed")(
    testM("failed") {
      val future: Future[Nothing] = ???

      testFuture(future.recover(t => t)) { result =>
        assertTrue(result.left.get.asInstanceOf[BadTime] == BadTime("Oh no!"))
      }
    }
  ) @@ ignore

  /** ✏ EXERCISE
    *
    * Use `Future#recover` to recover from a failed future.
    */

  val testRecover = suite("recover")(
    testM("recover") {

      val maybeInt: Future[Int] =
        Future {
          val randomInt = scala.util.Random.nextInt()
          if (randomInt % 2 == 0) randomInt
          else throw new Exception("It's Odd!")
        }

      val recovered: Future[Int] =
        maybeInt // <- EDIT HERE

      testFuture(recovered) { result =>
        assertTrue(result.right.get % 2 == 0)
      }
    } @@ nonFlaky
  ) @@ ignore

  override def exercise =
    suite("Future")(
      testSuccessful,
      testFailed,
      testRecover
    )
}
