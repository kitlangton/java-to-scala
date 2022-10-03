package course.cohort3

import zio.test._
import zio.{RuntimeFlags => _, _}

object ZioTestExample extends ZIOSpecDefault {

  val expensiveCalculation: UIO[Int] =
    Random.nextInt
      .debug("CALCULATED")
      .delay(1.second)
      .withClock(Clock.ClockLive)

  def spec =
    suite("Example")(List.tabulate(10)(n => testExample(n))) // @@ TestAspect.parallelN(10)

  def testExample(n: Int) =
    test(s"example $n") {
      for {
        _    <- ZIO.debug(s"START TEST $n")
        nums <- ZIO.collectAllPar(List.fill(5)(expensiveCalculation))
      } yield assertTrue(nums.length > 3)
    }
}
