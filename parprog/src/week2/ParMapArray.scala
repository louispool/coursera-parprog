package week2

/**
  * Created by louis on 03/10/2016.
  */
object ParMapArray {

  import common._

  def mapASegSeq[A, B](inp: Array[A], left: Int, right: Int, f: A => B,
                       out: Array[B]) = {
    // Writes to out(i) for left <= i <= right-1
    var i = left
    while (i < right) {
      out(i) = f(inp(i))
      i = i + 1
    }
  }

  def threshold = 2

  def mapASegPar[A, B](inp: Array[A], left: Int, right: Int, f: A => B,
                       out: Array[B]): Unit = {
// Writes to out(i) for left <= i <= right-1
    if (right - left < threshold)
      mapASegSeq(inp, left, right, f, out)
    else {
      val mid = left + (right - left) / 2
      parallel(mapASegPar(inp, left, mid, f, out),
                mapASegPar(inp, mid, right, f, out))
    }
  }

  def main(args: Array[String]): Unit = {

    val in = Array(2, 3, 4, 5, 6)
    val out = Array(0, 0, 0, 0, 0)
    val f = (x: Int) => x * x

    //mapASegSeq(in, 1, 3, f, out)
    //println(out.deep.mkString(", "))

    mapASegPar(in, 0, 5, f, out)
    println(out.deep.mkString(", "))
  }
}
