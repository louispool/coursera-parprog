package week2

import common._

sealed abstract class Tree[A] {
  val size: Int
}

case class Leaf[A](a: Array[A]) extends Tree[A] {
  override val size = a.size
}

case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {
  override val size = l.size + r.size
}

object ParMapTree {

  def mapTreePar[A: Manifest, B: Manifest](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Leaf(a) => {
      val len = a.length;
      val b = new Array[B](len)
      var i = 0
      while (i < len) {
        b(i) = f(a(i));
        i = i + 1
      }
      Leaf(b)
    }
    case Node(l, r) => {
      val (lb, rb) = parallel(mapTreePar(l, f), mapTreePar(r, f))
      Node(lb, rb)
    }
  }
}

