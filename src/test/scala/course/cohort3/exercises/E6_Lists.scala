package course.cohort3.exercises

import course.archive.cohort2.exercises.Exercise
import zio.test.TestAspect._
import zio.test._

/** For loops are common in other programming languages. In functional Scala,
  * however, most for loops are replaced by operations on Scala collections,
  * which provide high-level and declarative solutions to many problems
  * involving data processing and transformation.
  *
  * Mastery of the Scala collections library can provide an enormous boost to
  * productivity, as well as cement understanding of the tools of the
  * experienced functional programmer (mapping, filtering, folding, etc.).
  *
  * In this module, you will become more familiar with the power of the Scala
  * collections library as you solve a variety of common tasks using the
  * collections operations.
  */

object ListOperations extends Exercise {

  /** ✏ EXERCISE
    *
    * Using `List#foreach`, add up the contents of the provided list into the
    * variable `sum`.
    */
  val foreachTest = test("foreach") {
    var sum = 0

    val list = List(0, 3, 0, 2, 1)

    list.foreach { _ =>
      sum += 0
    }

    assertTrue(sum == 6)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `List#map`, transform the provided list into a new list, where each
    * element of the list has been multiplied by 2.
    */
  val mapTest = test("map") {
    val list1 = List(0, 3, 0, 2, 1)

    val list2: List[Int] = ???

    assertTrue(
      list1 == List(0, 3, 0, 2, 1),
      list2 == List(0, 6, 0, 4, 2),
      list2.sum == 12,
      list2.length == list1.length
    )
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `List#filter`, filter the provided list so that it only contains
    * even numbers.
    */
  val filterTest = test("filter") {
    val list1 = List(0, 3, 0, 2, 1)

    val list2 = list1.filter(_ => true) // EDIT HERE

    assertTrue(list2 == List(0, 0, 2))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Use `List#take` to take the first 2 elements of the provided list.
    */
  val takeTest = test("take") {
    val list1 = List(1, 2, 3, 4)

    val list2 = list1

    assertTrue(list2 == List(1, 2))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Use `List#takeWhile` to take elements for as long as they are strictly
    * less than 3.
    */
  val takeWhileTest = test("takeWhile") {
    val list1 = List(1, 2, 0, 3, 1, 2)

    val list2 = list1

    assertTrue(list2 == List(1, 2, 0))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Use `List#drop` to drop the first 2 elements of the provided list.
    */
  val dropTest = test("drop") {
    val list1 = List(1, 2, 3, 4)

    val list2 = list1

    assertTrue(list2 == List(3, 4))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Use `List#dropWhile` to drop elements for as long as they are strictly
    * less than 3.
    */
  val dropWhileTest = test("dropWhile") {
    val list1 = List(1, 2, 0, 3, 1, 2)

    val list2 = list1

    assertTrue(list2 == List(3, 1, 2))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `List#collect` and a partial function, collect all even numbers from
    * the provided List, wrapping them into an `Even` wrapper type.
    */
  val collectTest = test("collect") {
    final case class Even(number: Int)

    val isEven = (i: Int) => i % 2 == 0

    val list1 = List(0, 3, 0, 2, 1)

    def list2: List[Even] = list1.collect(???)

    assertTrue(list2 == List(Even(0), Even(0), Even(2)))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `partition`, partition the provided list of integers into those that
    * are even, and those that are odd.
    */
  val partitionTest = test("partition") {
    val isEven = (i: Int) => i % 2 == 0

    val list = List(0, 3, 0, 2, 1)

    val (even, odd) = list.partition(_ => ???)

    assertTrue(even == List(0, 0, 2) && odd == List(3, 1))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `List#find`, find the first number that is greater than two in the
    * provided list.
    */
  val findTest = test("find") {
    val list = List(1, 2, 3, 4)

    def firstGreaterThan2: Option[Int] = ???

    assertTrue(firstGreaterThan2 == Some(3))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `List#exists`, test to see if there exists an element of the list
    * that is negative.
    */
  val existsTest = test("exists") {
    val list = List(1, 2, 3, 4, -1, 5)

    def existsNegative: Boolean = ???

    assertTrue(existsNegative)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `List#forall`, test to see if all elements of the list are even
    * numbers.
    */
  val forallTest = test("forall") {
    val isEven = (i: Int) => i % 2 == 0

    val list = List(0, 2, 6, 8, 12, 10)

    def forallEven: Boolean = ???

    assertTrue(forallEven)
  } @@ ignore

  def exercise =
    suite("Operations")(
      foreachTest,
      mapTest,
      filterTest,
      takeTest,
      takeWhileTest,
      dropTest,
      dropWhileTest,
      collectTest,
      partitionTest,
      findTest,
      existsTest,
      forallTest
    )

}

object Folds extends Exercise {

  /** ✏ EXERCISE
    *
    * Using `List#foldLeft`, compute the sum of a list.
    */
  val sumTest = test("sum") {
    def sum(list: List[Int]): Int = ???

    assertTrue(sum(List(1, 2, 3, 4, 5)) == 15)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `List#foldLeft`, compute the maximum element of a list.
    */
  val maxTest = test("max") {
    def max(list: List[Int]): Int = ???

    assertTrue(max(List(1, 7, 3, 2, 4, 5)) == 7)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `List#foldLeft`, compute the minimum element of a list.
    */
  val minTest = test("min") {
    def min(list: List[Int]): Int = ???

    assertTrue(min(List(1, 7, 3, 2, 0, 4, 5)) == 0)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `List#foldLeft`, compute the reverse of a list.
    */
  val reverseTest = test("reverse") {
    def reverse[A](list: List[A]): List[A] = ???

    assertTrue(reverse(List(1, 7, 3)) == List(3, 7, 1))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `List#foldLeft`, implement a function to partition a list into those
    * satisfying a predicate, and those not satisfying the predicate.
    */
  val partitionTest = test("partition") {
    def partition[A](list: List[A])(pred: A => Boolean): (List[A], List[A]) = ???

    assertTrue(partition(List(1, 7, 3))(_ < 5) == ((List(1, 3), List(7))))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `List#foldLeft`, implement a function to take `n` elements from a
    * list.
    */
  val takeTest = test("take") {
    def take[A](n: Int, list: List[A]): List[A] = ???

    assertTrue(take(2, List(1, 7, 3)) == List(1, 7))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Using `List#foldLeft`, implement a function to take elements from a list
    * for as long as a predicate is satisfied.
    */
  val takeWhileTest = test("takeWhile") {
    def takeWhile[A](list: List[A])(pred: A => Boolean): List[A] = ???

    assertTrue(takeWhile(List(1, 7, 3))(_ < 5) == List(1))
  } @@ ignore

  def exercise =
    suite("Folds")(
      sumTest,
      maxTest,
      minTest,
      reverseTest,
      partitionTest,
      takeTest,
      takeWhileTest
    )
}

object Performance extends Exercise {

  /** ✏ EXERCISE
    *
    * Investigate and fix the performance problem with this code merely by
    * changing the collection type used.
    */
  val headTailTest = test("head/tail") {
    def sum(values: Seq[Int]): Int =
      values.headOption match {
        case None        => 0
        case Some(value) => value + sum(values.drop(1))
      }

    assertTrue(sum(0 to 10000) > 0)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Investigate and fix the performance problem with this code merely by
    * changing the collection type used.
    */
  val randomAccessTest = test("random access") {
    def sumProduct(left: Seq[Int], right: Seq[Int]): Int = {
      val length = left.length.max(right.length)

      (0 to length).foldLeft(0) { case (sum, index) =>
        sum + left(index) * right(index)
      }
    }

    assertTrue(sumProduct(List.fill(1000)(2), List.fill(1000)(2)) > 0)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Investigate and fix the performance problem with this code merely by
    * changing the collection type used.
    */
  val containmentTest = test("containment") {
    def equivalent(left: Seq[Int], right: Seq[Int]): Boolean =
      left.forall(i => right.contains(i)) &&
        right.forall(i => left.contains(i))

    assertTrue(equivalent(List.fill(1000)(2), List.fill(1000)(2)))
  } @@ ignore

  def exercise =
    suite("Performance")(
      headTailTest,
      randomAccessTest,
      containmentTest
    )
}
