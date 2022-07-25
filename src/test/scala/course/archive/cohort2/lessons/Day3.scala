package course.lessons

// 1. Purity (Totality)
// 2. Case Classes v. Classes = Reference vs Value Semantics

object ByNameRedux extends App {

  def ifThenElse[A](condition: Boolean, ifTrue: => A, ifFalse: => A): A =
    if (condition) ifTrue
    else ifFalse

  val result: Int = ifThenElse(
    condition = true,
    ifTrue = 10,
    ifFalse = throw new Error("OH NO")
  )

  println(result)
}

object MultipleParameterListRedux extends App {

  case class Pair[A](first: A, second: A)

  def combinePair[A](pair: Pair[A])(combine: (A, A) => A): A =
    combine(pair.first, pair.second)

  // TODO: Remove type ascriptions
  // The other reason form multiple parameter lists
  def combineInts(pair: Pair[Int]): Int =
    combinePair(pair)(_ + _)

  def combineStrings(pair: Pair[String]): String =
    combinePair(pair)(_ ++ _)

  val pair = Pair(10, 20)
  println(combineInts(pair))

  val pair2 = Pair("Hello", "World")
  println(combineStrings(pair2))
//  def combineBooleans(pair: Pair[Boolean]): Boolean =
//    combinePair(pair, _ && _)
}
