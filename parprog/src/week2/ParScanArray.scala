package week2

import common._

class ParScanArray {

  //Implemented in parallel
  def reduceSeg1[A](inp: Array[A], left: Int, right: Int,
                    a0: A, f: (A, A) => A): A = ???

  def mapSeg[A, B](inp: Array[A], left: Int, right: Int,
                   fi: (Int, A) => B,
                   out: Array[B]): Unit = ???

  def scanLeft[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = {

    out(0) = a0
    var a = a0

    var i = 0
    while (i < inp.length) {
      a = f(a, inp(i))
      i = i + 1
      out(i) = a
    }
  }

  def scanLeftPar[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]) = {

    val fi = { (i: Int, v: A) => reduceSeg1(inp, 0, i, a0, f) }

    mapSeg(inp, 0, inp.length, fi, out)

    val last = inp.length - 1
    out(last + 1) = f(out(last), inp(last))
  }

  /**
   * Parallelization of scanLeft using Trees
   *
   * Example here is:
   * List (1, 3, 8, 50).scanLeft(100)((s, x) => s + x) == List(100, 101, 104, 112, 162)
   */
  sealed abstract class Tree[A]
  case class Leaf[A](a: A) extends Tree[A]
  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]


  sealed abstract class TreeRes[A] { val res: A  }
  case class LeafRes[A](override val res: A) extends TreeRes[A]
  case class NodeRes[A](l: TreeRes[A], override val res: A, r: TreeRes[A]) extends TreeRes[A]

  /**
   *      o        f(): (_+_)      62
   *     / \                      /  \
   *    o   o         ==>        4    58
   *   / \ / \                  / \  /  \
   *  1  3 8 50                1  3  8   50
   */
  def reduceRes[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
    case Leaf(v) => LeafRes(v)
    case Node(l, r) => {
      val (tL, tR) = (reduceRes(l, f), reduceRes(r, f))
      NodeRes(tL, f(tL.res, tR.res), tR)
    }
  }

  /**
   * Parallel version of reduceRes(called upsweep to indicate bottom-up computation)
   */
  def upsweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
    case Leaf(v) => LeafRes(v)
    case Node(l, r) => {
      val (tL, tR) = parallel(upsweep(l, f), upsweep(r, f))
      NodeRes(tL, f(tL.res, tR.res), tR)
    }
  }

  /**
   * Next we need a tree for 100, 101, 104, 112, 162
   *
   * downsweep does the following:
   *
   *     62       f(): downsweep where a0=100           62
   *    /  \                                        /        \
   *   4    58                ==>               100+4        104+58
   *  / \  /  \                                 /   \        /     \
   * 1  3  8   50                            100+1  101+3  104+8  112+50
   *                                         =101   =104   =112   =162
   */
  def downsweep[A](t: TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = t match {
    case LeafRes(a) => Leaf(f(a0, a)) // ’a0’ is reduce of all elements left of the tree ’t’
    case NodeRes(l, _, r) => {
      val (tL, tR) = parallel(downsweep[A](l, a0, f),
                               downsweep[A](r, f(a0, l.res), f))
      Node(tL, tR)
    }
  }

  def prepend[A](x: A, t: Tree[A]): Tree[A] = t match {
    case Leaf(v) => Node(Leaf(x), Leaf(v))
    case Node(l, r) => Node(prepend(x, l), r)
  }

  def scanLeft[A](t: Tree[A], a0: A, f: (A, A) => A): Tree[A] = {
    val tRes = upsweep(t, f)
    val scan1 = downsweep(tRes, a0, f)
    prepend(a0, scan1)
  }
}
