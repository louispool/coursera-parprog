package week3

import java.util.concurrent.ForkJoinTask

import common._

/**
 * The combiner contract:
 * ▶ calling combine returns a new combiner that contains elements of input combiners
 * ▶ calling combine leaves both original Combiners in an undefined state
 * ▶ combine is an efficient method – O(log n) or better
 */
trait Combiner[A, Repr] extends Builder[A, Repr] {

  def combine(that: Combiner[A, Repr]): Combiner[A, Repr]
}


//trait ParTraversable[T] extends Traversable[T] with Splitter[T] {
//
//  override def split: Seq[ParTraversable[T]]
//  def newCombiner: Combiner[T, ParTraversable[T]]
//
//  def parFilter(p: T => Boolean): Traversable[T] = {
//    if (remaining < threshold) filter(p)
//    else {
//      val children: Seq[ForkJoinTask[T]] = for (child <- split) yield task {
//        child.parFilter(p)
//      }
//      //children.map(_.join()).foldLeft(z)(f)
//    }
//    val c = newCombiner
//    for (x <- this) if (p(x)) c += x
//    c.result
//  }
//}