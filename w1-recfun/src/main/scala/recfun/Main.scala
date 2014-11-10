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
  def pascal(c: Int, r: Int): Int =
    if (c < 0 || r < 0 || c > r) 0
    else if (c == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
    
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
  {
      def count(n: Int, char: Char): Int = {
        if (char == '(') n + 1 
        else if (char == ')') n - 1 
        else n
      }
      
      def balanceHelper(chars: List[Char], numberOfOpen: Int) : Boolean =
      {
        if (chars.isEmpty) numberOfOpen == 0
        else if (numberOfOpen < 0) false
        else balanceHelper(chars.tail, count(numberOfOpen, chars.head))
      }
	  balanceHelper(chars, 0) 
  }
    

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
  	if (money < 0) 0
  	else if (coins.isEmpty) 0
  	else if (money == 0) 1
  	else countChange(money - coins.head, coins) + countChange(money, coins.tail)
}
