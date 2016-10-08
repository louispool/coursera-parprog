package week1

import common._

object SumSegment {

  val threshold: Int = 1

  def pNorm(a: Array[Int], p: Double): Double = math.pow(sumSegment(a, p, 0, a.length), 1 / p)

  def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Double = {
    var sum = 0d
    for (i <- s until t) {
      sum = sum + math.pow(a(i), p)
    }
    sum
  }

  def pNormTwoPart(a: Array[Int], p: Double): Double = {
    val m = a.length / 2
    val (sum1, sum2) = parallel(sumSegment(a, p, 0, m), sumSegment(a, p, m, a.length))
    math.pow(sum1 + sum2, 1 / p)
  }

  def pNormRec(a: Array[Int], p: Double): Double = math.pow(sumSegmentRec(a, p, 0, a.length), 1 / p)

  def sumSegmentRec(a: Array[Int], p: Double, s: Int, t: Int): Double = {
    if (t - s < threshold) {
      sumSegment(a, p, s, t)
    } else {
      val m = s + (t - s)/2
      val (sum1, sum2) = parallel(sumSegmentRec(a, p, s, m), sumSegmentRec(a, p, m, t))
      sum1 + sum2
    }
  }
}
