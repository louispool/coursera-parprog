package week3

import java.util.concurrent.ForkJoinTask

import common._

/**
 * The splitter contract:
 * ▶ after calling split, the original splitter is left in an undefined state
 * ▶ the resulting splitters traverse disjoint subsets of the original splitter
 * ▶ remaining is an estimate on the number of remaining elements
 * ▶ split is an efficient method – O(log n) or better
 */
trait Splitter[T] extends Iterator[T] {

  def split: Seq[Splitter[T]]
  def remaining: Int

  def threshold: Int

  def fold(z: T)(f: (T, T) => T): T = {
    if (remaining < threshold) foldLeft(z)(f)
    else {
      val children: Seq[ForkJoinTask[T]] = for (child <- split) yield task { child.fold(z)(f) }
      children.map(_.join()).foldLeft(z)(f)
    }
  }
}


