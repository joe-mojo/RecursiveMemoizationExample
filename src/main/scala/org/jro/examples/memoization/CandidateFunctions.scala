package org.jro.examples.memoization

object CandidateFunctions {
	def square(x: Int): Long = x.toLong * x

	def factorial(n: Int): BigInt = if(n < 2) 1 else BigInt(n) * factorial(n - 1)
}
