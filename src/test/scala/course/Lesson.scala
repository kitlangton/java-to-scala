package course

import zio.test._
import zio.test.environment.TestEnvironment

trait Lesson extends DefaultRunnableSpec {
  def exercise: ZSpec[TestEnvironment, Any]

  override def spec: ZSpec[TestEnvironment, Any] = exercise
}

object Lesson {
  type ???
}
