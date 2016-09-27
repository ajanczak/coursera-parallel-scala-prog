package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

sealed trait Tree

case object Nil extends Tree

case class Node[T](value: T, left: Tree = Nil, right: Tree = Nil) extends Tree {
  override def toString() = {
    s"\t[$value]" +
      s"\n[${left}]\t\t[$right]"
  }
}

case class Money(amount: Int, coins: List[Int])


object ParallelCountChange {

  private def buildChildren(m: Money) = {
    val moneyCnt = m.amount - m.coins.head
    val left =
      if (moneyCnt <= 0) Node(Money(moneyCnt, m.coins))
      else Nil
    val right =
      if (m.coins.size > 1) Node(Money(m.amount, m.coins.tail))
      else Nil
    val result = Node(m, left, right)
    //println(s"buildChildren $result")
    result
  }

  private def inspectTree(node: Tree, result: Int): Int = {
    println(s"inspectTree:\n $node")
    node match {
      case Nil => 0
      case Node(value: Money, left, right) =>
        if (value.amount == 0) {
          1
        }
        else {
          val node = buildChildren(value)
          val leftValue = inspectTree(node.left, result)
          val rightValue = inspectTree(node.right, result)
          leftValue + rightValue
        }
      case x => throw new Exception(s"Unmatched case: $x")
    }
  }

  /** Returns the number of ways change can be made from the specified list of
    * coins for the specified amount of money.
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money > 0 && coins.nonEmpty) {
      val root = buildChildren(Money(money, coins))
      inspectTree(root, 0)
    }
    else 0
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
    * specified list of coins for the specified amount of money.
    */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    ???
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold =
    ???

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    ???


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    ???
  }
}
