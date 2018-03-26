package org.jro.examples.memoization

import org.jro.examples.memoization.CandidateFunctions.{PseudoRecursive, factorial}

import scala.annotation.tailrec

object CandidateFunctions {
	def square(x: Int): Long = x.toLong * x

	def longRunningSquare(x: Int): Long = {
		val res = x.toLong * x
		for (i <- 0 to 100000) { square(x) }
		res
	}

	def factorial(n: Int): BigInt = if(n < 2) 1 else BigInt(n) * factorial(n - 1)

	def fibonacci(n: Int): Int =
		if (n==0) 0
		else if (n < 3) 1
		else fibonacci(n - 2) + fibonacci(n - 1)

	private object PseudoRecursive {

		def factorial(n: Int, recurse: Int => BigInt): BigInt = {
			if(n < 2) 1 else BigInt(n) * recurse(n - 1)
		}

		def fibonacci(n: Int, recurse:  Int => Int): Int = {
			if(n==0) 0
			else if(n < 3) 1
			else recurse(n - 2) + recurse(n - 1)
		}

	}

	val mfactorial: (Int)=> BigInt = Memoize.Y(CandidateFunctions.PseudoRecursive.factorial)

	val mfibonacci: (Int)=> Int = Memoize.Y(CandidateFunctions.PseudoRecursive.fibonacci)

	def fibonacci_tailrec(n: Int): Int = {
		@tailrec
		def fiborec(a1: Int, a2: Int, n: Int): Int = {
			if(n == 0) a1
			else if(n == 1) a2
			else fiborec(a2, a1 + a2, n - 1)
		}
		fiborec(0, 1, n)
	}
}
