package org.jro.examples

import java.time.{Clock, Duration}

package object memoization {
	val clock = Clock.systemDefaultZone()

	def timed[T, R](f: T => R): T => (R, Duration) = {
		(arg: T) => {
			val start = clock.instant()
			val res: R = f(arg)
			val stop = clock.instant()
			val d = Duration.between(start, stop)
			(res, d)
		}
	}

	class CountCalls[-T, +R](f: T => R) extends (T => R) {
		private[this] var counter = 0
		override def apply(v1: T): R = {
			counter += 1
			f(v1)
		}
		def callCount: Int = counter
	}
	object CountCalls {
		def apply[T, R](f: T => R): CountCalls[T, R] = new CountCalls(f)
	}
}
