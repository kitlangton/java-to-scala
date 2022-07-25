package course.lessons.projects

import com.twitter.util.Future

case class NoPersonException(c: Int, m: Some[String]) extends Exception {
  val code: Int               = c
  val message: Option[String] = m
}

case class MyPerson(name: String)

object ErrorHandling extends App {

  // this is a person service
  def getPersonImpl(name: String): Future[String] =
    if (name == "Johnny") Future.exception(new RuntimeException("OOPSIE"))
    else Future.exception(NoPersonException(1, Some("User not exist")))

  def getPerson(name: String): Future[MyPerson] =
    getPersonImpl(name).map(MyPerson)

  // here's my code ===============
  // Throwing `getPerson`
  // Future[MyPerson] => Future[Option[MyPerson]]

  def futureToOption[A](future: Future[A]): Future[Option[A]] =
    future.map(Some(_)).handle {
      case _: NoPersonException => None
      case error                => throw error
    }

  def myFunction(name: String): Future[Option[String]] =
    futureToOption(getPerson(name).map(_.name))

  myFunction("Johnny").onSuccess(result => println(result))
}
