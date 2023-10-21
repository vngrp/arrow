package arrow.core

import kotlin.test.Test
import arrow.core.Either.Left
import arrow.core.Either.Right
import arrow.core.test.any
import arrow.core.test.either
import arrow.core.test.intSmall
import arrow.core.test.laws.MonoidLaws
import arrow.core.test.nonEmptyList
import arrow.core.test.suspendFunThatReturnsAnyLeft
import arrow.core.test.suspendFunThatReturnsAnyRight
import arrow.core.test.suspendFunThatReturnsEitherAnyOrAnyOrThrows
import arrow.core.test.suspendFunThatThrows
import arrow.core.test.testLaws
import io.kotest.assertions.fail
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.matchers.shouldBe
import io.kotest.property.Arb
import io.kotest.property.arbitrary.boolean
import io.kotest.property.arbitrary.byte
import io.kotest.property.arbitrary.char
import io.kotest.property.arbitrary.double
import io.kotest.property.arbitrary.float
import io.kotest.property.arbitrary.int
import io.kotest.property.arbitrary.list
import io.kotest.property.arbitrary.long
import io.kotest.property.arbitrary.nonPositiveInt
import io.kotest.property.arbitrary.short
import io.kotest.property.arbitrary.string
import io.kotest.property.checkAll
import kotlinx.coroutines.test.runTest

class EitherTest {
  
  val ARB = Arb.either(Arb.string(), Arb.int())

  @Test fun monoidLawsEither() = runTest {
    testLaws(
      MonoidLaws("Either", 0.right(), { x, y -> x.combine(y, String::plus, Int::plus) }, ARB)
    )
  }
    
     @Test fun isLeftShouldReturnTrueIfLeftAndFalseIfRight() = runTest {
      checkAll(Arb.int()) { a: Int ->
        val x = Left(a)
        if (x.isLeft()) x.value shouldBe a
        else fail("Left(a).isLeft() cannot be false")
        x.isRight() shouldBe false
      }
    }
    
     @Test fun isRightShouldReturnFalseIfLeftAndTrueIfRight() = runTest {
      checkAll(Arb.int()) { a: Int ->
        val x = Right(a)
        if (x.isRight()) x.value shouldBe a
        else fail("Right(a).isRight() cannot be false")
        x.isLeft() shouldBe false
      }
    }
    
     @Test fun tapAppliesEffectsReturningTheOriginalValue() = runTest {
      checkAll(Arb.either(Arb.long(), Arb.int())) { either ->
        var effect = 0
        val res = either.onRight { effect += 1 }
        val expected = when (either) {
          is Left -> 0
          is Right -> 1
        }
        effect shouldBe expected
        res shouldBe either
      }
    }
    
     @Test fun tapLeftAppliesEffectsReturningTheOriginalValue() = runTest {
      checkAll(Arb.either(Arb.long(), Arb.int())) { either ->
        var effect = 0
        val res = either.onLeft { effect += 1 }
        val expected = when (either) {
          is Left -> 1
          is Right -> 0
        }
        effect shouldBe expected
        res shouldBe either
      }
    }
    
     @Test fun foldShouldApplyFirstOpIfLeftAndSecondOpIfRight() = runTest {
      checkAll(Arb.intSmall(), Arb.intSmall()) { a, b ->
        val right: Either<Int, Int> = Right(a)
        val left: Either<Int, Int> = Left(b)
        
        right.fold({ it + 2 }, { it + 1 }) shouldBe a + 1
        left.fold({ it + 2 }, { it + 1 }) shouldBe b + 2
      }
    }

     @Test fun combineTwoRightsShouldReturnARightOfTheCombineOfTheInners() = runTest {
      checkAll(Arb.string(), Arb.string()) { a: String, b: String ->
        Right(a + b) shouldBe Right(a).combine(
          Right(b),
          Int::plus,
          String::plus
        )
      }
    }
    
     @Test fun combineTwoLeftsShouldReturnALeftOfTheCombineOfTheInners() = runTest {
      checkAll(Arb.string(), Arb.string()) { a: String, b: String ->
        Left(a + b) shouldBe Left(a).combine(
          Left(b),
          String::plus,
          Int::plus
        )
      }
    }
    
     @Test fun combineARightAndALeftShouldReturnLeft() = runTest {
      checkAll(Arb.string(), Arb.string()) { a: String, b: String ->
        Left(a) shouldBe Left(a).combine(Right(b), String::plus, String::plus)
        Left(a) shouldBe Right(b).combine(Left(a), String::plus, String::plus)
      }
    }
    
     @Test fun getOrElseShouldReturnValue() = runTest {
      checkAll(Arb.int(), Arb.int()) { a: Int, b: Int ->
        Right(a).getOrElse { b } shouldBe a
        Left(a).getOrElse { b } shouldBe b
      }
    }
    
     @Test fun getOrNullShouldReturnValue() = runTest {
      checkAll(Arb.int()) { a: Int ->
        Right(a).getOrNull() shouldBe a
      }
    }
    
     @Test fun getOrNoneShouldReturnSomeValue() = runTest {
      checkAll(Arb.int()) { a: Int ->
        Right(a).getOrNone() shouldBe Some(a)
      }
    }
    
     @Test fun getOrNoneShouldReturnNoneWhenLeft() = runTest {
      checkAll(Arb.string()) { a: String ->
        Left(a).getOrNone() shouldBe None
      }
    }

     @Test fun swapShouldInterchangeValues() = runTest {
      checkAll(Arb.int()) { a: Int ->
        Left(a).swap() shouldBe Right(a)
        Right(a).swap() shouldBe Left(a)
      }
    }

     @Test fun mapShouldAlterRightInstanceOnly() = runTest {
      checkAll(Arb.intSmall(), Arb.intSmall()) { a, b ->
        val right: Either<Int, Int> = Right(a)
        val left: Either<Int, Int> = Left(b)
        
        right.map { it + 1 } shouldBe Right(a + 1)
        left.map { it + 1 } shouldBe left
      }
    }
    
     @Test fun mapLeftShouldAlterLeftInstanceOnly() = runTest {
      checkAll(Arb.intSmall(), Arb.intSmall()) { a, b ->
        val right: Either<Int, Int> = Right(a)
        val left: Either<Int, Int> = Left(b)
        
        right.mapLeft { it + 1 } shouldBe right
        left.mapLeft { it + 1 } shouldBe Left(b + 1)
      }
    }

     @Test fun flatMapShouldMapRightInstanceOnly() = runTest {
      checkAll(Arb.intSmall(), Arb.intSmall()) { a, b ->
        val right: Either<Int, Int> = Right(a)
        val left: Either<Int, Int> = Left(b)
        
        right.flatMap { Right(it + 1) } shouldBe Right(a + 1)
        left.flatMap { Right(it + 1) } shouldBe left
      }
    }

     @Test fun handleErrorWithShouldHandleLeftInstanceOtherwiseReturnRight() = runTest {
      checkAll(Arb.int(), Arb.string()) { a: Int, b: String ->
        Left(a).recover<Int, String, String> { Right(b).bind() } shouldBe Right(b)
        Right(a).recover { Right(a + 1).bind() } shouldBe Right(a)
        Left(a).recover { Left(b).bind() } shouldBe Left(b)
      }
    }
    
     @Test fun catchShouldReturnRightResultWhenFDoesNotThrow() = runTest {
      Either.catch { 1 } shouldBe Right(1)
    }
    
     @Test fun catchShouldReturnLeftResultWhenFThrows() = runTest {
      val exception = Exception("Boom!")
      Either.catch { throw exception } shouldBe Left(exception)
    }

   @Test fun zipOrAccumulateResultsInAllRightTransformedOrAllLeftCombinedAccordingToCombine() = runTest {
    checkAll(
      Arb.either(Arb.string(), Arb.short()),
      Arb.either(Arb.string(), Arb.byte()),
      Arb.either(Arb.string(), Arb.int()),
      Arb.either(Arb.string(), Arb.long()),
      Arb.either(Arb.string(), Arb.float()),
      Arb.either(Arb.string(), Arb.double()),
      Arb.either(Arb.string(), Arb.char()),
      Arb.either(Arb.string(), Arb.string()),
      Arb.either(Arb.string(), Arb.boolean())
    ) { a, b, c, d, e, f, g, h, i ->
      val res = Either.zipOrAccumulate({ e1, e2 -> "$e1$e2" }, a, b, c, d, e, f, g, h, i, ::Tuple9)
      val all = listOf(a, b, c, d, e, f, g, h, i)

      val expected = if (all.any { it.isLeft() }) {
        all.filterIsInstance<Left<String>>().fold("") { acc, t -> "$acc${t.value}" }.left()
      } else {
        all.filterIsInstance<Right<Any?>>().map { it.value }.let {
          Tuple9(it[0], it[1], it[2], it[3], it[4], it[5], it[6], it[7], it[8]).right()
        }
      }

      res shouldBe expected
    }
  }

   @Test fun zipOrAccumulateWithoutSemigroupResultsInAllRightTransformedOrAllLeftInANonEmptyList() = runTest {
    checkAll(
      Arb.either(Arb.string(), Arb.short()),
      Arb.either(Arb.string(), Arb.byte()),
      Arb.either(Arb.string(), Arb.int()),
      Arb.either(Arb.string(), Arb.long()),
      Arb.either(Arb.string(), Arb.float()),
      Arb.either(Arb.string(), Arb.double()),
      Arb.either(Arb.string(), Arb.char()),
      Arb.either(Arb.string(), Arb.string()),
      Arb.either(Arb.string(), Arb.boolean())
    ) { a, b, c, d, e, f, g, h, i ->
      val res = Either.zipOrAccumulate(a, b, c, d, e, f, g, h, i, ::Tuple9)
      val all = listOf(a, b, c, d, e, f, g, h, i)

      val expected = if (all.any { it.isLeft() }) {
        all.filterIsInstance<Left<String>>().map { it.value }.toNonEmptyListOrNull()!!.left()
      } else {
        all.filterIsInstance<Right<Any?>>().map { it.value }.let {
          Tuple9(it[0], it[1], it[2], it[3], it[4], it[5], it[6], it[7], it[8]).right()
        }
      }

      res shouldBe expected
    }
  }

   @Test fun zipOrAccumulateEitherNelResultsInAllRightTransformedOrAllLeftInANonEmptyList() = runTest {
    checkAll(
      Arb.either(Arb.nonEmptyList(Arb.int()), Arb.short()),
      Arb.either(Arb.nonEmptyList(Arb.int()), Arb.byte()),
      Arb.either(Arb.nonEmptyList(Arb.int()), Arb.int()),
      Arb.either(Arb.nonEmptyList(Arb.int()), Arb.long()),
      Arb.either(Arb.nonEmptyList(Arb.int()), Arb.float()),
      Arb.either(Arb.nonEmptyList(Arb.int()), Arb.double()),
      Arb.either(Arb.nonEmptyList(Arb.int()), Arb.char()),
      Arb.either(Arb.nonEmptyList(Arb.int()), Arb.string()),
      Arb.either(Arb.nonEmptyList(Arb.int()), Arb.boolean())
    ) { a, b, c, d, e, f, g, h, i ->
      val res = Either.zipOrAccumulate(a, b, c, d, e, f, g, h, i, ::Tuple9)
      val all = listOf(a, b, c, d, e, f, g, h, i)

      val expected = if (all.any { it.isLeft() }) {
        all.filterIsInstance<Left<NonEmptyList<String>>>()
          .flatMap { it.value }
          .toNonEmptyListOrNull()!!.left()
      } else {
        all.filterIsInstance<Right<Any?>>().map { it.value }.let {
          Tuple9(it[0], it[1], it[2], it[3], it[4], it[5], it[6], it[7], it[8]).right()
        }
      }

      res shouldBe expected
    }
  }
}

@Suppress("RedundantSuspendModifier", "UNUSED_PARAMETER")
suspend fun handleWithPureFunction(a: Any, b: Any): Either<Throwable, Any> =
  b.right()

@Suppress("RedundantSuspendModifier", "UNUSED_PARAMETER")
suspend fun handleWithPureFunction(throwable: Throwable): Either<Throwable, Unit> =
  Unit.right()

@Suppress("RedundantSuspendModifier", "UNUSED_PARAMETER")
private suspend fun <A> throwException(
  a: A,
): Either<Throwable, Any> =
  throw RuntimeException("An Exception is thrown while handling the result of the supplied function.")
