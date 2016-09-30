package week1

/**
  * Created by louis on 29/09/2016.
  */
object MonteCarloPI {

  import scala.util.Random

  def mcCount(iter: Int): Int = {

    val randomX = new Random
    val randomY = new Random

    var hits = 0
    for (i <- 0 until iter) {
      val x = randomX.nextDouble // in [0,1]
      val y = randomY.nextDouble // in [0,1]
      if (x * x + y * y < 1) hits = hits + 1
    }
    hits
  }

  def monteCarloPiSeq(iter: Int): Double = 4.0 * mcCount(iter) / iter

  def monteCarloPiPar(iter: Int): Double = {
    val ((pi1, pi2), (pi3, pi4)) = parallel(
                                             parallel(mcCount(iter / 4), mcCount(iter / 4)),
                                             parallel(mcCount(iter / 4), mcCount(iter - 3 * (iter / 4))))
    4.0 * (pi1 + pi2 + pi3 + pi4) / iter
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = ???
}
