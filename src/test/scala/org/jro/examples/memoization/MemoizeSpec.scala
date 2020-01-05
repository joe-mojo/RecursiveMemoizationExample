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
			"call original function only once for each value" in {
				import CandidateFunctions._
				var callCount = 0
				def pseudoRecFibonacci(n: Int, recurse:  Int => Int): Int = {
					callCount += 1
					if(n==0) 0
					else if(n < 3) 1
					else recurse(n - 2) + recurse(n - 1)
				}

				var fibonacci: Int => Int = null
				fibonacci = pseudoRecFibonacci(_, fibonacci)

				fibonacci(5) shouldBe 5
				callCount shouldBe 9

				callCount = 0
				val memoizedFibonacci = Memoize.Y(pseudoRecFibonacci)
				memoizedFibonacci(5) shouldBe 5
				callCount shouldBe 5
				//f(5) = f(4) + f(3) ; calls = 1
				//|      f(4) = f(3) + f(2) ; calls = 2
				//|      |      f(3) = f(2) + f(1) ; calls = 3
				//|      |      |      f(2) = 1 ; calls = 4
				//|      |      |      f(1) = 1 ; calls = 5
				//|      |      2    + f(2) = cache(2) = 1 => no call ; calls = 5
				//|      3   +  f(3) = cache(3) = 2 => no call ; calls = 5
				//5
				//
			}
		}
	}
}
