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
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000000
    val chars = new Array[Char](length)
    val threshold = 100000
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

  @tailrec
  private def iterateBalance(chars: List[Char], stack: List[Char]): Boolean = {
    //println(s"Iteration: chars: ${chars.toString()}, stack: $stack")
    chars match {
      case c if c.isEmpty => stack.isEmpty
      case c if c.head == LeftPar => iterateBalance(c.tail, (stack :+ c.head))
      case c if c.head == RightPar =>
        stack.nonEmpty && iterateBalance(c.tail, stack.dropRight(1))
      case c => iterateBalance(c.tail, stack)
    }
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    iterateBalance(chars.toList, List())
  }

  case class Result(sum: Int, left: Int, right: Int)


  def recBalance(chars: Array[Char], threshold: Int): Result = {
    val res = chars.toList match {
      case _ if chars.isEmpty => Result(0,0,0)
      case list if list.size > threshold =>
        val (left, right) = chars.splitAt(chars.size / 2)
        val (rL, rR) = parallel(recBalance(left, threshold), recBalance(right, threshold))
        Result(rL.sum + rR.sum, rL.sum, rR.sum)
      case _ =>
        val openParRes = chars.filter(_ == LeftPar).size
        val closeParRes = chars.filter(_ == RightPar).size
        val sum  = openParRes - closeParRes
        val parrOpt = chars.dropWhile(x => x != LeftPar && x != RightPar).headOption
        val firstLeft = parrOpt.map(x => if (x == RightPar) -1 else 1).getOrElse(0)
        Result(sum, firstLeft, sum)
    }
    //println(s"recBalance chars ${chars.toList}, Result:$res")
    res
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    val result = recBalance(chars, threshold)
    if (result.left < 0) false
    else if (result.sum != 0) false
    else true
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
