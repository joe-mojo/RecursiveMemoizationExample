package org.jro.examples.memoization

class Memoize[-T, +R](f: T => R) extends (T => R) {
	import scala.collection.mutable
	private[this] val cache = mutable.Map.empty[T, R]

	def apply(x: T): R = {
		cache getOrElseUpdate(x, f(x))
	}
}

object Memoize {
	def apply[T, R](f: T => R) = new Memoize(f)

	def Y[T, R](f: (T, T => R) => R) = {
		var yf: T => R = null
		yf = Memoize(f(_, yf))
		yf
	}
}
