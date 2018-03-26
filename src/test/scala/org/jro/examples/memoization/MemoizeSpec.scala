package org.jro.examples.memoization

import java.time.Duration

import org.scalatest.{FreeSpec, Matchers}

import scala.util.Random

class MemoizeSpec extends FreeSpec with Matchers{

	"Memoized function" - {
		"should have the same result as original function" in {
			import CandidateFunctions._
			val memoizedSquare = Memoize(square)
			val memoizedFactorial = Memoize(factorial)
			val memoizedFibonacci = Memoize(fibonacci)

			for (_ <- 0 to 1000; rnd = Random.nextInt()) {
				square(rnd) shouldBe memoizedSquare(rnd)
			}
			for(_ <- 0 to 1000; rnd = Random.nextInt(2000)) {
				factorial(rnd) shouldBe memoizedFactorial(rnd)
			}

			//fibonacci is much more complex, 42 is the answer.
			for(_ <- 0 to 42; rnd = Random.nextInt(42)) {
				val res = fibonacci(rnd)
				res shouldBe memoizedFibonacci(rnd)
			}

		}

		"should be faster than original for long running functions" in {
			import CandidateFunctions._
			val timedSquare = timed(longRunningSquare)
			val memoizedTimedSquare = timed(Memoize(longRunningSquare))

			val nbIter = 5000
			val avgOriginalDuration = (for (i <- 0 to nbIter) yield timedSquare(5)).foldLeft(Duration.ZERO) { (d, result) =>
				d.plus(result._2)
			}.dividedBy(nbIter)

			val avgMemoizedDuration = (for (i <- 0 to nbIter) yield memoizedTimedSquare(5)).foldLeft(Duration.ZERO) { (d, result) =>
				d.plus(result._2)
			}.dividedBy(nbIter)

			avgMemoizedDuration.compareTo(avgOriginalDuration) should be < 0

		}

		"will be faster for recursive function only if repeated" in {
			import CandidateFunctions._
			val timedFibo = timed(fibonacci)
			val timedMemoizedFibo = timed(Memoize(fibonacci))

			val nbIter = 1000
			val avgOriginalDuration = (for (i <- 0 to nbIter) yield timedFibo(21)).foldLeft(Duration.ZERO) { (d, result) =>
				d.plus(result._2)
			}.dividedBy(nbIter)

			val avgMemoizedDuration = (for (i <- 0 to nbIter) yield timedMemoizedFibo(21)).foldLeft(Duration.ZERO) { (d, result) =>
				d.plus(result._2)
			}.dividedBy(nbIter)

			avgMemoizedDuration.compareTo(avgOriginalDuration) should be < 0

			timedMemoizedFibo(44)._2.compareTo(timedFibo(44)._2) should be >= 0
		}

		"should call original non-recursive function only once for each call" in {
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
			memoizedFunction(5) shouldBe 25
			callCountingSquare.callCount shouldBe 2
		}

		"memoized with Y combinator should" - {
			"call original function only once for each call" in {
				import CandidateFunctions._
				//val callCountingFibonacci = Memoize.Y(C)
				pending
			}
		}
	}
}
