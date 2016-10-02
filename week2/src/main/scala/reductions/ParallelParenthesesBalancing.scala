package reductions

import scala.annotation._
import org.scalameter._
import common._

import scala.collection.immutable.Stack

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 25,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

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

  val LeftPar = '('
  val RightPar = ')'


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    //print(s"chars: ${chars.toList}")
    var i = 0
    var counter = 0
    while (i < chars.length) {
      if (chars(i) == LeftPar) counter += 1
      else if (chars(i) == RightPar) counter -= 1
      //println(s"counter: $counter")
      i += 1
      if (counter < 0) return false
    }
    if (counter == 0) true
    else false
  }

  case class Result(sum: Int, left: Int, right: Int)


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def recBalance(from: Int, to: Int): (Int, Int) = {

      def balanceSeq(from: Int, to: Int): (Int, Int)  = {
        //print(s"chars: ${chars.toList}")
        var i = from
        var counter = 0
        var unbalancedClose = 0
        while (i < to) {
          if (chars(i) == LeftPar) counter += 1
          else if (chars(i) == RightPar) counter -= 1
          //println(s"counter: $counter")
          i += 1
          //          if (counter < 0) {
          //            unbalancedClose += 1
          //            println(s"adding unbalance, unbalancedClose: $unbalancedClose")
          //          }
        }
        if (counter < 0) {
          unbalancedClose += 1
        }
        //println(s"---balanceSeq: ${(counter,unbalancedClose)}")
        (counter,unbalancedClose)
      }

      if (to < from) throw new Exception("eee")

      val howMany = to - from
      val res = if (howMany > threshold) {
        val chunkSize = (to - from) / 2
        //println(s"Starting recursion for threshold:$threshold [${chars.toList}],\nhowMany:$howMany\nchunk: $chunkSize\n LEFT: [$from - ${from+chunkSize}]\n RIGHT: [${from+chunkSize} - $to]")
        //readLine()
        val (leftSide, rightSide) = parallel(recBalance(from, from+chunkSize), recBalance(from+chunkSize, to))
        //if (leftSide._2 > 0 ) println(s"------ !!! Unbalanced LEFT: ${leftSide._2}")
        //if (rightSide._2 > 0 ) println(s"------ !!! Unbalanced RIGHT: ${rightSide._2}")
        (leftSide._1 + rightSide._1, leftSide._2)

        //Result(rL.sum + rR.sum, rL.sum, rR.sum)
      }
      else {
        balanceSeq(from, to)
      }
      //println(s"recBalance chars ${chars.toList}, Result:$res")
      res
    }

    val (sum, unbalanced) = recBalance(0, chars.size)
    if (sum == 0 && unbalanced <= 0) true
    else false

  }


  // For those who want more:
  // Prove that your reduction operator is associative!

}
