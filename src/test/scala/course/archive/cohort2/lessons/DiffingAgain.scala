package course.lessons

import java.lang.reflect.Field
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

/** Used to acquire the diffs between instances of the same case class.
  * Currently, tested for all primitive types, Strings, Options[T], and fields
  * that are other case classes This does not work for fields that are of type
  * [[Collection]], currently.
  */
object DiffingUtils2 extends App {

  /** Used to get the list of field names that are different between two
    * instances of a case class returns empty list if no differences where
    * found.
    * @param p1
    * @param p2
    * @param tag
    *   implicit parameter
    * @tparam A
    *   an instance of a class that extends [[Product]]. Hint: All case classes
    *   extend Product
    * @return
    */
  def compareProducts[A <: Product](p1: A, p2: A)(implicit tag: ClassTag[A]): List[String] = {
    val listBuffer = ListBuffer.empty[String]
    compareProducts[A](listBuffer, p1, p2, None)
    listBuffer.result()
  }

  /** Will directly compare two Seq (ie. seq1[0] compared to seq2[0], seq1[1]
    * compared to seq2[1]) and return the list of different fields between each
    * object in the Seq. Currently, it will return a list of string that has the
    * following format "[$index].$fieldName" where index is the index where the
    * two items were different.
    * @param seq
    * @param seq2
    * @param tag
    *   implicit parameter
    * @tparam A
    *   an instance of a class that extends [[Product]]. Hint: All case classes
    *   extend Product
    * @return
    *   a list of strings that display the index that the differing
    */
  def compareLists[A <: Product](seq: Seq[A], seq2: Seq[A])(implicit tag: ClassTag[A]): List[String] = {
    val listBuffer = ListBuffer.empty[String]
    val iter1      = seq.iterator
    val iter2      = seq2.iterator
    var index      = 0
    while (iter1.hasNext && iter2.hasNext) {
      val product1 = iter1.next()
      val product2 = iter2.next()
      compareProducts[A](listBuffer, product1, product2, Option(index))
      index += 1
    }
    listBuffer.result()
  }
  private def compareProducts[A <: Product](listBuffer: ListBuffer[String], p1: A, p2: A, index: Option[Int])(implicit
      tag: ClassTag[A]
  ): Unit = {
    val values1: Iterator[Any]  = p1.productIterator
    val values2: Iterator[Any]  = p2.productIterator
    val fields: Iterator[Field] = tag.runtimeClass.getDeclaredFields.iterator
    while (fields.hasNext && values1.hasNext && values2.hasNext) {
      val field: Field      = fields.next()
      val fieldName: String = field.getName
      val value1            = values1.next()
      val value2            = values2.next()
      val valuesEqual = (value1, value2) match {
        case (arr: Array[_], arr2: Array[_]) => arr.sameElements(arr2)
        case _                               => value1 == value2
      }
      if (!valuesEqual) {
        listBuffer.append(index match {
          case None => s"$fieldName"
          case _    => s"[${index.get}].$fieldName"
        })
      }
    }
  }

  final case class Blob(name: String, friends: Array[String] = Array("Billy"))
  println(compareProducts[Blob](Blob("test"), Blob("test2")))
  println(
    compareLists[Blob](
      Seq(Blob("test"), Blob("test2")),
      Seq(Blob("test", Array("Billy2")), Blob("test"))
    )
  )
}
