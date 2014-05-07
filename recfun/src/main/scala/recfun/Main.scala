package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def isOpened(char: Char): Boolean = {
      char.toString.equals("(")
    }

    def isClosed(char: Char): Boolean = {
      char.toString.equals(")")
    }

    def cntClosed(chars: List[Char], cntOpened: Int): Int = {
      if (chars.isEmpty || cntOpened < 0) cntOpened
      else {
        val cntOpenedTmp = if (isOpened(chars.head)) cntOpened + 1
        else if (isClosed(chars.head)) cntOpened - 1
        else cntOpened
        cntClosed(chars.tail, cntOpenedTmp)
      }
    }

    cntClosed(chars, 0) == 0;
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val ways = Array.fill(money + 1)(0)
    ways(0) = 1
    coins.foreach(coin =>
      for (j <- coin to money) {
        ways(j) = ways(j) + ways(j - coin)
      })

    ways(money)

  }
}
