package week3

/**
 * The builder contract:
 * ▶ calling result returns a collection of type Repr, containing the
 *   elements that were previously added with +=
 * ▶ calling result leaves the Builder in an undefined state
 */
trait Builder[A, Repr] {

  def +=(elem: A): Builder[A, Repr]
  def result: Repr
}

trait Traversable[T] {

  def foreach(f: T => Unit): Unit
  def newBuilder: Builder[T, Traversable[T]]

  def filter(p: T => Boolean): Traversable[T] = {

    val b = newBuilder
    for (x <- this) if (p(x)) b += x
    b.result
  }
}

