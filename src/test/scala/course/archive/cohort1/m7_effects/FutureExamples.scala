package course.archive.cohort1.m7_effects

import FutureUtils.debugThread

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.Try

object LazyObject {
  val name = {
    println("INITIALIZING LAZY OBJECT `name`")
    "George Wilkerson"
  }
}

object LazyVal extends App {
  lazy val name = {
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

  private val mapped: Future[Option[Int]] = future
    .map { string =>
      debugThread("map")
      println(s"Got result: $string")
      string.toIntOption
    }

  val result: Future[Int] =
    mapped
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

  val composed: Future[List[String]] = for {
    file          <- downloadFile("file.txt")
    config        <- getConfig("file.txt")
    processedFile <- processFile(file)
  } yield processedFile.take(config.linesRequired)

  def timed[A](future: Future[A]): Future[(A, Int)] = ???

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
  //      .recover { case e: ArithmeticException =>
  //        println(s"Computation failed with '$e'")
  //        -999
  //      }

  computation
    .foreach { result =>
      println(s"\nResult: $result")
    }

  val value: Try[Int] =
    Try(Await.result(computation, Duration.Inf))

  println(value.toEither)

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

  val url1 = getUrlFuture(
    "https://gist.githubusercontent.com/kitlangton/478a1b57a95cd57a5154c3897d3d3090/raw/88cd025b5cbd581c421f581e26f484d6298178cd/gistfile1.txt"
  )

  val url2 = getUrlFuture(
    "https://gist.githubusercontent.com/kitlangton/3b8f2063ce2bc72467bc3d86aea91d2f/raw/41f686dc749eda789e578e42f13970ee6c93b4f6/idea-live-templates.xml"
  )

  (url1 zip url2).foreach(println)
  Await.result(url1, Duration.Inf)
  //  println(s"URL 1 Length: ${url1.length}")
  //  println(s"URL 2 Length: ${url2.length}")

}
