package arrow.core

import kotlin.test.Test
import arrow.core.test.laws.MonoidLaws
import arrow.core.test.testLaws
import io.kotest.property.Arb
import io.kotest.property.arbitrary.boolean
import kotlinx.coroutines.test.runTest

class BooleanTest {
  @Test fun booleanMonoidLaws() = runTest {
    testLaws(
      MonoidLaws("Boolean", true, { x, y -> x && y }, Arb.boolean())
    )
  }
}
