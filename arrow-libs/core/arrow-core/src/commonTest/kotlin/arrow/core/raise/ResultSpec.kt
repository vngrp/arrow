package arrow.core.raise

import kotlin.test.Test
import io.kotest.matchers.shouldBe
import kotlinx.coroutines.test.runTest

@Suppress("UNREACHABLE_CODE")
class ResultSpec {
  val boom = RuntimeException("Boom!")

  @Test fun resultException() = runTest {
    result {
      throw boom
    } shouldBe Result.failure(boom)
  }

  @Test fun resultSuccess() = runTest {
    result { 1 } shouldBe Result.success(1)
  }

  @Test fun resultRaise() = runTest {
    result { raise(boom) } shouldBe Result.failure(boom)
  }

  @Test fun recoverWorksAsExpected() = runTest {
    result {
      val one: Int = recover({ Result.failure<Int>(boom).bind() }) { 1 }
      val two = Result.success(2).bind()
      one + two
    } shouldBe Result.success(3)
  }
}
