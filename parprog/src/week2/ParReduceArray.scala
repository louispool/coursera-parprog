package week2

import common._

object ParReduceArray {

  def threshold = 5

  def reduceSeg[A](inp: Array[A], left: Int, right: Int, f: (A, A) => A): A = {
    if (right - left < threshold) {
      var res = inp(left)
      var i = left + 1
      while (i < right) {
        res = f(res, inp(i)); i = i + 1
      }
      res
    } else {
      val mid = left + (right - left) / 2
      val (a1, a2) = parallel(reduceSeg(inp, left, mid, f),
                              reduceSeg(inp, mid, right, f))
      f(a1, a2)
    }
  }
  def reduce[A](inp: Array[A], f: (A, A) => A): A =
    reduceSeg(inp, 0, inp.length, f)
}
