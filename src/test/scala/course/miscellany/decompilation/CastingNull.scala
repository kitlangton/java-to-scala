package course.miscellany.decompilation

object CastingNull extends App {
  println(null.asInstanceOf[Boolean])
  println(null.asInstanceOf[Int])
  println(null.asInstanceOf[Double])
  println(null.asInstanceOf[Char])
}
