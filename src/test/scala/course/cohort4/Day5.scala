package course.cohort4

class Day5 {}

object CaseClassCat extends App {
  // INSTRUCTIONS
  // ============
  // Define a method or two on the Cat case class that will modify it somehow, returning a new Cat.

  case class Cat(name: String, mood: Double, numLives: Int = 9) {
    def changeNameToFrank: Cat = copy(name = "Frank")

    def resetName(): Unit = println("I'm not changing my name!")
  }

  val cat = Cat("Felix", 0.5)
  println(cat.changeNameToFrank)
//  println(Cat.catWithOneLessLife(cat))
//  println(Cat.happierCat(cat))
//  println(cat.catWithOneLessLife)
//  println(cat.happierCat)

  // INSTRUCTIONS
  // ============
  // Factor out the duplication in the following methods by using generics.
  def intBaton(int: Int): Int             = int
  def stringBaton(string: String): String = string
  def catBaton(cat: Cat): Cat = {
    var catVar = cat
    catVar = catVar.copy(name = "Frank")
    cat
  }

  def anyBaton(any: Any): Any = any
  def baton[A](input: A): A   = input

  def genericBaton[A](a: A): A = a
  def genBaton[A](baton: A): A = baton
  def uniform[A](input: A): A  = input
//  def anyBaton[A](value: A): A = value

  val bools = List(true, false, true, false)
  println(bools.filter(a => a))

  println(identity(100))
  println(100)
//  val hooray: Int = identity(100)
//  val hooray: Int = 100

  val int: Int = uniform(1)

//  def baton[A](baton: A): A = baton

  //  def genericBaton[A](someVar: A): A = { someVar = 12
//  }

  val ex4: Int = genBaton(10)

  val ex1: Cat = catBaton(cat)
  val ex3: Cat = genericBaton(cat)
  val ex5: Int = genericBaton(10)
//  def anyBaton[A](input: A): A = input

}

object ParametricPolymorphismPracticePartTwo extends App {
  // INSTRUCTIONS
  // ============
  // Factor out the duplication in the following case classes and their corresponding methods by using generics.
  final case class Cat(name: String)

  final case class Box[A](value: A)
  // poly/monomorphic
  // poly - morphic
  // mono - morphic
  def extractBox[A](box: Box[A]): A = box.value
  println(extractBox(Box(10)))
  println(extractBox(Box("Hello")))
  println(extractBox(Box(Cat("Felix"))))
}

object AndThen extends App {
// A |> (A => B) |> (B => C)
// 10 |> stringify(_) |> uppercased(_)

  def pipeline[Start, Middle, End](
      startToMiddle: Start => Middle,
      middleToEnd: Middle => End
  ): Start => End =
    startToMiddle andThen middleToEnd
  // ps I don't have intelliJ set up, my b

  def pipeline1[Start, Middle, End](
      startToMiddle: Start => Middle,
      middleToEnd: Middle => End
  ): Start => End = { x: Start =>
    middleToEnd(startToMiddle(x))
  }

  def pipeline0[A, B, C](
      f: A => B,
      g: B => C
  ): A => C =
    (a: A) => g(f(a))

//  def pipeline[Start, Middle, End](
//      startToMiddle: Start => Middle,
//      middleToEnd: Middle => End
//  ): Start => End =
//    startToMiddle.andThen(middleToEnd)

  def pipelineC(
      stringLength: String => Int,
      isEven: Int => Boolean
  ): String => Boolean =
    stringLength.andThen(isEven)

  def stringLength(string: String): Int = string.length
  def isEven(int: Int): Boolean         = int % 2 == 0

//  val f: String => Boolean = pipeline(stringLength, isEven)
//  println(f("hello"))
//  println(f("hi"))
}

object FunctionExploration extends App {

  trait NumberOperation {
    def operation(int: Int): Int
  }

  val addOne = new NumberOperation {
    def operation(int: Int): Int = int + 1
  }

  val addOne0: NumberOperation = (int: Int) => int + 1
  val square: NumberOperation  = (int: Int) => int * int

  // Single Abstract Method (SAM) conversion
  trait ==>[In, Out] {
    def apply(in: In): Out

    def twice: In ==> Out =
      in => {
        this.apply(in)
        this.apply(in)
      }

    def andThen[Out2](that: Out ==> Out2): In ==> Out2 =
      (in: In) => that(this(in))
  }

  val intToString: Int => String =
    (x: Int) => x.toString

  // SINGLE Abstract Method
  val stringLength: String ==> Int =
    (string: String) => string.length

  val isEven: Int ==> Boolean =
    (int: Int) => int % 2 == 0

  val stringIsEven: String ==> Boolean =
    stringLength andThen isEven

  val debug: String ==> Unit =
    (string: String) => println(string)

  val debugTwice = debug.twice.twice.twice
  debugTwice("WHATEVER")

}

// - functions from scratch
// - finish predicate
// - implicits
