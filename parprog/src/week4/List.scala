package week4

sealed trait List[+T] {
  def head: T
  def tail: List[T]
}

case class ::[T](head: T, tail: List[T]) extends List[T]

case object Nil extends List[Nothing] {
  def head = sys.error("empty list")
  def tail = sys.error("empty list")
}

class ListImpl[T] {

  def filter(lst: List[T])(p: T => Boolean): List[T] = lst match {
    case x :: xs if p(x) => ::(x, filter(xs)(p))
    case x :: xs => filter(xs)(p)
    case Nil => Nil
  }
}



