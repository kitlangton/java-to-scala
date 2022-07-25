package course.lessons

class Person(val name: String, val age: Int)

object Person {
  def apply(name: String, age: Int): Person =
    new Person(name, age)

  val hello = List(1, 2, 3, 5)
}

object Day4 extends App {
  val person  = new Person("John", 30)
  val person2 = Person("John", 30)
  val person3 = Person.apply("John", 30)
  List(1, 2, 3)
}
