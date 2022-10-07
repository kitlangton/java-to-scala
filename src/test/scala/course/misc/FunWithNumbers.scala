package course.misc

object FunWithNumbers {
  class Number(var value: Int) {
    def ＋(other: Number): Int = { value += other.value; value }
  }

  object Number {
    private var cache = Map.empty[Int, Number]

    def make(int: Int): Number =
      cache.getOrElse(
        int, {
          val number = new Number(int)
          cache += (int -> number)
          number
        }
      )

  }

  implicit def int2Number(i: Int): Number = Number.make(i)

  def println(n: Number): Unit = scala.Predef.println(n.value)
}
