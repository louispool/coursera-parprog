package week4

sealed trait Conc[+T] {

  def level: Int
  def size: Int
  def left: Conc[T]
  def right: Conc[T]
}

case object Empty extends Conc[Nothing] {
  def level = 0
  def size = 0
}


class Single[T](val x: T) extends Conc[T] {
  def level = 0
  def size = 1
}

/**
 * 1. A <> node can never contain Empty as its subtree.
 * 2. The level difference between the left and the right subtree of a <>
 *    node is always 1 or less. E.g.
 *
 *     x                                    x
 *    / \                                 /   \
 *   0   2                               1     2
 *      / \     (wrong)                 / \   / \      (right)
 *     1   0                           0   0 0   1
 *    / \                                       / \
 *   0   0                                     0   0
 */
case class <>[T](left: Conc[T], right: Conc[T]) extends Conc[T] {

  val level = 1 + math.max(left.level, right.level)
  val size  = left.size + right.size

  def <>(that: Conc[T]): Conc[T] = {
    if (this == Empty) that
    else if (that == Empty) this
    else concat(this, that)
  }

  /**
   *  Concat needs to produce balanced trees
   */
  def concat(xs: Conc[T], ys: Conc[T]): Conc[T] = {
    val diff = ys.level - xs.level
    if (diff >= -1 && diff <= 1) new <>(xs, ys)
    else if (diff < -1) {
      if (xs.left.level >= xs.right.level) {
        val nr = concat(xs.right, ys)
        new <>(xs.left, nr)
      } else {
        val nrr = concat(xs.right.right, ys)
        if (nrr.level == xs.level - 3) {
          val nl = xs.left
          val nr = new <>(xs.right.left, nrr)
          new <>(nl, nr)
        } else {
          val nl = new <>(xs.left, xs.right.left)
          val nr = nrr
          new <>(nl, nr)
        }
      }
    }
  }

}
