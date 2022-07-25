package course.lessons

import course.lessons.DiffingUtils.{compareLists, compareProducts}

import scala.collection.mutable.ListBuffer

object Comparing extends App {
  // case classes
  // fields

  // Array("pet.name")
  final case class Person(name: String, age: Int, pet: Pet)
  final case class Pet(name: String)

  val people1 = List(
    Person("Alice", 25, Pet("Fluffy")),
    Person("Alice", 25, Pet("Fluffy")),
    Person("Alice", 25, Pet("Fluffy")),
    Person("Alice", 25, Pet("Fluffy"))
  )

  // "[3].age"
  val people2 = List(
    Person("Alice", 25, Pet("Fluffy")),
    Person("Alice", 25, Pet("Fluffy")),
    Person("Alice", 25, Pet("Fluffy")),
    Person("Alice", 26, Pet("Fluffy"))
  )

  final case class Blob(name: String, age: Int, pet: Pet) {
    def getDifferingFields(that: Blob): List[String] = {
      val listBuffer = ListBuffer.empty[String]
      if (name != that.name) listBuffer += "name"
      if (age != that.age) listBuffer += "age"
      listBuffer.result()
    }
  }

  private val alice25 = Blob("Alice", 25, Pet("Fluffy"))
  private val alice28 = Blob("Alice", 28, Pet("Stinky"))

  val blobs1 = List(alice28, alice25, alice25)
  val blobs2 = List(alice25, alice25, Blob("Kyle", 25, Pet("Stinky")))

  println(compareLists(blobs1, blobs2))
}

object DiffingUtils {

  def compareProducts[A <: Product](idx: Int, listBuffer: ListBuffer[String], p1: A, p2: A): Unit = {
    val values1: Iterator[Any]       = p1.productIterator
    val values2: Iterator[Any]       = p2.productIterator
    val fieldNames: Iterator[String] = p1.productElementNames

    while (fieldNames.hasNext) {
      val name = fieldNames.next()
      if (values1.next() != values2.next())
        listBuffer += s"$idx.$name"
    }
  }

  def compareLists[A <: Product](l1: List[A], l2: List[A]): List[String] = {
    val listBuffer = ListBuffer.empty[String]
    val iter1      = l1.iterator
    val iter2      = l2.iterator
    var idx        = 0

    while (iter1.hasNext && iter2.hasNext) {
      val blob1 = iter1.next()
      val blob2 = iter2.next()
      compareProducts(idx, listBuffer, blob1, blob2)
      idx += 1
    }

    listBuffer.result()
  }

}
