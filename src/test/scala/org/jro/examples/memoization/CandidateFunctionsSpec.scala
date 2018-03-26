package org.jro.examples.memoization

import org.jro.examples.memoization.CandidateFunctions._
import org.scalatest.{FreeSpec, Matchers}

import scala.util.Random

class CandidateFunctionsSpec extends FreeSpec with Matchers{
	"fibonacci" - {
		"should give the fibonnacci suite" - {
			"when head recursive" in {
				(0 to 10) map fibonacci should contain theSameElementsInOrderAs Seq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
			}
			"when tail recursive" in {
				(0 to 10) map fibonacci_tailrec should contain theSameElementsInOrderAs Seq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
			}
		}
	}

	"Counting.fibonacci" - {
		"should give the fibonacci suite recurse a lot" in {
			Counting.fibonaciCalls = 0
			(0 to 8).map(Counting.fibonacci) should contain theSameElementsInOrderAs Seq(0, 1, 1, 2, 3, 5, 8, 13, 21)
			Counting.fibonaciCalls shouldBe 101

			Counting.fibonaciCalls = 0
			Counting.fibonacci(6) shouldBe 8
			Counting.fibonaciCalls shouldBe 15
		}
	}

	"square" - {
		"should give the square of the input value when no overflow occurs" in {
			for(_ <- 0 to 1000; rnd = Random.nextInt(Math.sqrt(Long.MaxValue).toInt)) {
				square(rnd) shouldBe rnd.toLong * rnd
			}
		}
	}

	"factorial" - {
		"should give the factorial of the input value" in {
			(0 to 10) map factorial should contain theSameElementsInOrderAs Seq(1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800)
		}
	}
}
