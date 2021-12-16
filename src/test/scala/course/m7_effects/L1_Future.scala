package course.m7_effects

import course.Lesson
import zio.test._

import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import FutureUtils._
import zio.test.TestAspect.{ignore, nonFlaky}

object LazyObject {
  val name = {
    println("INITIALIZING LAZY OBJECT `name`")
    "George Wilkerson"
  }
}

object LazyVal extends App {
  val name = {
    println("INITIALIZING `name`")
    "George Wilkerson"
  }
}

object ByNameParameters extends App {

  def ifElse[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  ifElse(true, println("IT'S TRUE"), println("IT'S FALSE"))

}

object FutureExample1 extends App {

  /** Step 1 - Create a Future
    *
    * The body of `Future.apply` is executed asynchronously in a different
    * thread. This future will complete with the result of the body, when the
    * body completes.
    */
  val future: Future[String] = Future {
    debugThread("future")
    println("Starting future!")
    Thread.sleep(1000)
    println("Completing future!")
    "Hello"
  }

  val result: Future[Int] =
    future
      .map { string =>
        debugThread("map")
        println(s"Got result: $string")
        string.toIntOption
      }
      .flatMap { optionInt =>
        Future {
          println("Starting Get or Else")
          debugThread("getOrElse")
          Thread.sleep(1000)
          println("Completing Get or Else")
          optionInt.getOrElse(0)
        }
      }

  debugThread("main")

  /** We must wait for the future to complete, otherwise the program will exit.
    * This is because the app will exit when the main thread finishes.
    */
  Await.result(result, Duration.Inf)
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
    println(s"\nDownloading $url!")
    Thread.sleep(2000)
    val source = scala.io.Source.fromURL(url)
    val lines  = source.mkString
    source.close()
    println(s"Downloaded $url!")
    Thread.sleep(500)
    lines
  }

  // future version
  def getUrlFuture(url: String): Future[String] = Future {
    getUrl(url)
  }

  val url1 = getUrl(
    "https://gist.githubusercontent.com/kitlangton/478a1b57a95cd57a5154c3897d3d3090/raw/88cd025b5cbd581c421f581e26f484d6298178cd/gistfile1.txt"
  )

  val url2 = getUrl(
    "https://gist.githubusercontent.com/kitlangton/3b8f2063ce2bc72467bc3d86aea91d2f/raw/41f686dc749eda789e578e42f13970ee6c93b4f6/idea-live-templates.xml"
  )

  println(s"URL 1 Length: ${url1.length}")
  println(s"URL 2 Length: ${url2.length}")

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

  /** ✏ EXERCISE
    *
    * Use a for comprehension to compose the existing functions to download the
    * contents of a URL, save it to the "database", and return its line count.
    */
  def downloadData(url: String): Future[String] =
    Future {
      println(s"Downloading $url...")
      val source = scala.io.Source.fromURL(url)
      val lines  = source.mkString
      source.close()
      println(s"Downloaded $url!")
      lines
    }

  def asyncLineCount(text: String): Future[Int] =
    Future {
      println(s"Counting lines...")
      Thread.sleep(1000)
      println(s"Counted lines!")
      text.split("\n").length
    }

  def saveToDatabase(text: String): Future[Unit] =
    Future {
      println(s"Saving text to database...")
      Thread.sleep(1000)
      println(s"Saved text to database!")
      database = Some(text)
    }

  private var database: Option[String] = None

  val testForComprehension = suite("for comprehension")(
    testM("for comprehension") {

      val url =
        "https://gist.githubusercontent.com/kitlangton/478a1b57a95cd57a5154c3897d3d3090/raw/88cd025b5cbd581c421f581e26f484d6298178cd/gistfile1.txt"

      // COMPLETE THIS DEFINITION USING A FOR COMPREHENSION
      // HINT: Use `downloadData`, `asyncLineCount`, and `saveToDatabase`
      val future: Future[Int] = ???

      testFuture(future) { result =>
        assertTrue(
          result.right.get == 3,
          database.get ==
            """
What a beautiful file.
That's great.
One more line will do. 
""".trim
        )
      }
    }
  )

  override def exercise =
    suite("Future")(
      testSuccessful,
      testFailed,
      testRecover,
      testForComprehension
    )
}
