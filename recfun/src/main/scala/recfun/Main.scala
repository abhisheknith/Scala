package recfun

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
    if(r < c) 0
    else if(c == 0 ) 1
    else if (r == c) 1
    else if (Math.abs(r-c) == 1) r
    else pascal(c - 1, r - 1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def checkBalance(open : Int, remainingChars: List[Char]) : Boolean = {
      if(open < 0) false
      else if(remainingChars.isEmpty) open == 0
      else {
        if(remainingChars.head.equals('(')) {
          checkBalance(open + 1, remainingChars.tail)
        } else if(remainingChars.head.equals(')')) {
          checkBalance(open - 1, remainingChars.tail)
        } else {
          checkBalance(open, remainingChars.tail)
        }
      }
    }
    checkBalance(0, chars)
  }

  /**
  * Exercise 3
  */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) return 1
    else if(money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0
  }
}
