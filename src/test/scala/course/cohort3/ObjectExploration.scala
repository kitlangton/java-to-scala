package course.cohort3

// Try decompiling to Java (right click on the object name in the target folder in IDEA and select decompile)
object BagOfUtils {
  def add(x: Int, y: Int): Int = x + y
}

object BagOfUtils2 {
  def add(x: Int, y: Int): Int = x + y

  var name = "Hello"
}

object LazyStuff {
  lazy val x: Int = {
    println("I'm lazy")
    10
  }
}
