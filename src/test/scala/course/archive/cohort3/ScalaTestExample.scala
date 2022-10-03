package course.archive.cohort3

// TESTING

import course.cohort3.CasingTranslation.{camelCase, delayedCamelCase}
import org.scalatest.funspec._

import scala.concurrent.{Await, ExecutionContext, Future}

object CasingTranslation {
  // jim smith -> jimSmith
  // jim-smith -> jimSmith
  // jim_smith -> jimSmith
  def camelCase(s: String): String = {
    val words = s.split("[-_ ]").toList
    words match {
      case head :: tail => (head.toLowerCase :: tail.map(_.capitalize)).mkString
      case Nil          => ""
    }
  }

  def delayedCamelCase(s: String)(implicit ec: ExecutionContext): Future[String] =
    Future {
      Thread.sleep(1000)
      camelCase(s)
    }
}

class ScalaTestExample() extends AnyFunSpec {
  import scala.concurrent.ExecutionContext.Implicits.global

  describe("CasingTranslationSpecs") {
    it(s"should convert jim smith to jimSmith") {
      val nums = List(1, 2, 3)
      assert(nums(2) == 3)
    }

    it(s"sad") {
      val nums = List(1, 2, 3)
      assert(nums(2) == 3)
    }
  }
}

class ScalaAsyncTestExample() extends AsyncFunSpec {
  describe("CasingTranslationSpecs") {
    it("should convert jim smith to jimSmith") {
      Future {
        Thread.sleep(1000)
        assert(camelCase("jim smith") == "jimSmith")
        assert(camelCase("jim-smith") == "jimSmith")
        assert(camelCase("jim_smith") == "jimSmith")
      }
    }
  }
}
