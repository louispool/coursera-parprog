package week4

sealed trait Tree[+T]

case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
case class Leaf[T](elem: T) extends Tree[T]

case object Empty extends Tree[Nothing]

class TreeImpl[T] extends Tree[T] {

  def filter(t: Tree[T])(p: T => Boolean): Tree[T] = t match {
    case Node(left, right) => Node(filter(left)(p), filter(right)(p))
    case Leaf(elem) => if (p(elem)) t else Empty
    case Empty => Empty
  }
}

