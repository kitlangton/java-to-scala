package course.cohort4

object Day4 extends App {
  val double = (x: Int) => x * 2
// double(10) == 20

  val intToChar = (x: Int) => x.toChar
// intToChar(65) == 'A'

  val stringLength = (x: String) => x.length
// stringLength("HELLO") == 5

  val curriedAdd = (x: Int) => (y: Int) => x + y
// curriedAdd(10)(20) == 30

  val add10 = curriedAdd(10)
// add10(20) == 30

  val replicateCurried = (x: String) => (y: Int) => x * y
// replicateCurried("ha")(3) == "hahaha"

  val replicateUncurried = (x: String, y: Int) => x * y
// replicateUncurried("ha", 3) == "hahaha"
}
