package course.archive.cohort1.miscellany.hm

// Implement Some Functional Effects
// - Synchronous
class Task1[+A](value: () => A) {
  def runSync: A = value()

  override def toString: String =
    s"Task1($value)"

  def repeat(n: Int): Task1[A] =
    if (n <= 0) this
    else this *> repeat(n - 1)

  def *>[B](that: => Task1[B]): Task1[B] =
    this.zip(that).map(_._2)

  def <*[B](that: => Task1[B]): Task1[A] =
    this.zip(that).map(_._1)

  def map[B](f: A => B): Task1[B] =
    Task1 {
      f(value())
    }

  def flatMap[B](f: A => Task1[B]): Task1[B] =
    Task1 {
      f(value()).runSync
    }

  def zip[B](that: Task1[B]): Task1[(A, B)] =
    for {
      a <- this
      b <- that
    } yield (a, b)
}

object Task1 {
  def apply[A](a: => A): Task1[A] =
    new Task1(() => a)

  def debug(msg: => Any): Task1[Unit] =
    Task1 {
      println(msg)
    }
}
