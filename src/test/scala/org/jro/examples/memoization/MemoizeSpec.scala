package org.jro.examples.memoization

import org.scalatest.{FreeSpec, Matchers}

import scala.util.Random

class MemoizeSpec extends FreeSpec with Matchers{
	"Memoized function" - {
		"should have the same result as original function" in {
			import CandidateFunctions._
			val memoizedSquare = Memoize(square)
			val memoizedFactorial = Memoize(factorial)

			for (_ <- 0 to 1000; rnd = Random.nextInt()) {
				square(rnd) shouldBe memoizedSquare(rnd)
			}
			for(_ <- 0 to 1000; rnd = Random.nextInt(2000)) {
				factorial(rnd) shouldBe memoizedFactorial(rnd)
			}

		}

		"should call original function only once for each argument" in {
			import CandidateFunctions._
			val callCountingSquare = CountCalls(square)
			val memoizedFunction = Memoize(callCountingSquare)
			callCountingSquare.callCount shouldBe 0

			memoizedFunction(5) shouldBe 25
			callCountingSquare.callCount shouldBe 1
			memoizedFunction(5) shouldBe 25
			callCountingSquare.callCount shouldBe 1

			memoizedFunction(6) shouldBe 36
			callCountingSquare.callCount shouldBe 2
			memoizedFunction(6) shouldBe 36
			callCountingSquare.callCount shouldBe 2
		}
	}
}
