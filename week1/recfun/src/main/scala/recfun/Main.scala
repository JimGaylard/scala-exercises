package recfun

import scala.annotation.tailrec

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
      def factorial(x: Int): Int = {
        @tailrec
        def loop(acc: Int, n: Int): Int = {
          if (n == 0) acc
          else loop(acc * n, n-1)
        }
        loop(1, x)
      }

      factorial(r) / (factorial(c) * factorial(r - c))
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def matchedParens(acc: Int, chars: List[Char]): Int = {
        if (acc < 0) -1
        else if (chars.isEmpty) 0
        else if (chars.head == '(') matchedParens(acc + 1, chars.tail)
        else if (chars.head == ')') matchedParens(acc - 1, chars.tail)
        else matchedParens(acc, chars.tail)
      }
      matchedParens(0, chars) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty || money < 0) 0
      else if (money == 0) 1
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
