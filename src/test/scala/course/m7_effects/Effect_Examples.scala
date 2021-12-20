package course.m7_effects

import course.m7_effects.task._

import scala.concurrent.{ExecutionContext, Future}

trait IOApp {
  def run: Task[Unit]

  def main(args: Array[String]): Unit =
    run.runSync
}

object MyApp extends IOApp {
  def run: Task[Unit] = ???
}

object FunctionalEffectSystems extends App {

  val t2 = Task {
    Thread.sleep(1000)
    println("TASK END")
  }

  def taskFor(millis: Int): Task[Int] =
    for {
      _ <- Task {
             Thread.sleep(millis)
             println("TASK COOL")
           }
      _ <- t2
    } yield 100

  private val delayed: Task[Int] =
    taskFor(1000)

  def logBeforeAndAfter[A](task: Task[A]): Task[A] =
    for {
      _ <- Task.debug("ABOUT TO EXECUTE MY FANCY TASK")
      a <- task
      _ <- Task.debug("DONE EXECUTING MY FANCY TASK")
    } yield a

  def printTask(string: String): Task[Unit] =
    Task {
      println(string)
    }

  def delayedFuture(implicit ec: ExecutionContext) =
    Future {
      Thread.sleep(1000)
      "HELLO"
    }

  val wrappedFuture =
    Task.fromFuture(implicit ec => delayedFuture)

  println(wrappedFuture.timed.debug("RESULT").runSync)
}

object AsyncExample extends App {
  //

  val longRunningTask  = Task("BIG NUMBER").delay(3000).debug("1")
  val shortRunningTask = Task("SMALL NUMBER").delay(1000).debug("2")

  // LONG            -> SHORT    -> RESULT

  // LONG.fork SHORT    ->            ->    RESULT
  //           LONG            ->.join
  val composed =
    for {
      fiber <- longRunningTask.fork
      y     <- shortRunningTask
//      _      <- fiber.interrupt
      x <- fiber.join
    } yield (x, y)

  composed.timed.debug("RESULT").runSync

  val listOfInt = List(1, 2, 3, 4, 5)

//  composed race composed

  def processInt(int: Int): Task[Int] =
    Task.debug(s"STARTING TO PROCESS $int") *>
      Task(println(s"PROCESSED $int")).delay(1000).as(int + 10)

  val all = Task.foreach(listOfInt)(processInt)

}

trait Logger {
  def log(string: String): Task[Unit]
}

object Logger {
  def testLogger: Task[TestLogger] =
    Ref.make(List.empty[String]).map { ref =>
      TestLogger(ref)
    }
}

final case class ConsoleLogger() extends Logger {
  override def log(string: String): Task[Unit] =
    Task {
      println(s"LOG: $string")
    }
}

final case class TestLogger(ref: Ref[List[String]]) extends Logger {
  override def log(string: String): Task[Unit] =
    ref.update(string :: _) *>
      Task {
        println(s"LOG: $string")
      }
}

final case class Todo(id: Long, name: String, isCompleted: Boolean)

trait TodoRepo {
  def create(name: String): Task[Todo]

  def allTodos: Task[List[Todo]]

  def update(todo: Todo): Task[Unit]
}

object TodoRepo {

  def makeInMemory(logger: Logger): Task[TodoRepo] =
    Ref
      .make(Map.empty[Long, Todo])
      .map { ref =>
        InMemoryTodoRepo(logger, ref)
      }

}

final case class TodoList(list: List[Todo]) {
  def serialize: String =
    list
      .map { todo =>
        val completed = if (todo.isCompleted) "X" else "_"
        s"${todo.id} $completed ${todo.name}"
      }
      .mkString("\n")
}

object TodoList {

  def deserialize(string: String): Task[TodoList] = Task {
    val list = string
      .split("\n")
      .map {
        case s"$id $completed $name" =>
          Todo(id.toLong, name, completed == "X")
        case other =>
          throw new Error(s"THIS IS UNPROBABLE: $other")
      }
      .toList

    TodoList(list)
  }

  def main(args: Array[String]): Unit = {
    val contents = scala.io.Source.fromResource("todos.txt").mkString

    deserialize(contents).debug.runSync
  }
}

final case class InMemoryTodoRepo(logger: Logger, ref: Ref[Map[Long, Todo]]) extends TodoRepo {
  override def create(name: String): Task[Todo] =
    ref.modify { db =>
      val id   = db.size + 1
      val todo = Todo(id, name, false)
      todo -> db.updated(id, todo)
    } <* logger.log(s"CREATED $name")

  override def allTodos: Task[List[Todo]] =
    ref.get.map(db => db.values.toList) <* logger.log("ALL TODOS")

  override def update(todo: Todo): Task[Unit] =
    ref.update { db =>
      db.updated(todo.id, todo)
    } <* logger.log(s"UPDATED ${todo.name}")
}

// Purely Functional Shopping Cart
final case class TodoApp(todoRepo: TodoRepo) {

  val listOfTodos =
    List(
      "Learn Scala",
      "Learn FP",
      "Buy Milk",
      "Eat Food",
      "Sleep",
      "Repeat"
    )

  def test: Task[List[Todo]] =
    (for {
      _ <- Task.foreachPar(listOfTodos) { todo =>
             todoRepo
               .create(todo)
               .delay(1000)
           }
      todos <- todoRepo.allTodos
    } yield todos).debug("FINAL RESULT")
}

object TodoApp extends App {

  def program: Task[List[String]] =
    for {
      logger         <- Logger.testLogger
      repo           <- TodoRepo.makeInMemory(logger)
      _              <- TodoApp(repo).test
      loggedMessages <- logger.ref.get
      _              <- Task.debug(s"LOGS: $loggedMessages")
    } yield loggedMessages

  program.runSync
}
