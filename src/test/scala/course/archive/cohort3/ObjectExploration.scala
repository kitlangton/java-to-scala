package course.archive.cohort3

import pprint.pprintln

// Try decompiling to Java (right click on the object name in the target folder in IDEA and select decompile)
object BagOfUtils {
  def add(x: Int, y: Int): Int = x + y
}

object BagOfUtils2 {
  def add(x: Int, y: Int): Int = x + y

  var name = "Hello"
}

final case class Dude(name: String, age: Int)

class Duchess(name: String, age: Int)

object DudeTests extends App {
  private val john: Dude = Dude("John", 30)
  john.copy(name = "hello")
  pprintln(john.productElementNames.toList)
}

object LazyStuff {
  lazy val x: Int = {
    println("I'm lazy")
    10
  }
}
