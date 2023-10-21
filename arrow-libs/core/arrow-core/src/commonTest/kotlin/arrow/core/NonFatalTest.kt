package arrow.core

import kotlin.test.Test
import io.kotest.matchers.shouldBe
import kotlinx.coroutines.test.runTest

class NonFatalTest {
    val nonFatals: List<Throwable> =
      listOf(
        RuntimeException(),
        Exception(),
        Throwable(),
        NotImplementedError()
      )

     @Test fun testNonFatalsUsingInvoke() = runTest {
      nonFatals.forEach {
        NonFatal(it) shouldBe true
      }
    }

     @Test fun testNonFatalsUsingThrowableNonFatalOrThrow() = runTest {
      nonFatals.forEach {
        it.nonFatalOrThrow() shouldBe it
      }
    }
}
