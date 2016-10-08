package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {

    def traverse(idx: Int, balance: Int): Int = {
      if (idx >= chars.length || balance < 0) balance
      else if (chars(idx) == '(')
        traverse(idx + 1, balance + 1)
      else if (chars(idx) == ')')
        traverse(idx + 1, balance - 1)
      else
        traverse(idx + 1, balance)
    }
    traverse(0, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, openBraces: Int, closeBraces: Int): (Int, Int) = {
      if (idx >= until) (openBraces, closeBraces)
      else if (chars(idx) == '(')
        traverse(idx + 1, until, openBraces + 1, closeBraces)
      else if (chars(idx) == ')') {
        if (openBraces > 0)
          traverse(idx + 1, until, openBraces - 1, closeBraces)
        else
          traverse(idx + 1, until, openBraces, closeBraces + 1)
      }
      else traverse(idx + 1, until, openBraces, closeBraces)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold)
        traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val ((o1, c1), (o2, c2)) = parallel(reduce(from, mid),
                                            reduce(mid, until))
        if (o1 >= c2)
          ((o1 - c2) + o2, c1)
        else
          (o2, (c2 - o1) + c1)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
