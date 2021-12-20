package course.m7_effects

import course.Lesson
import zio.test.TestAspect.{ignore, nonFlaky}
import zio.test._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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

      val future: Future[Int] = ???

      testFuture(future) { result =>
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
