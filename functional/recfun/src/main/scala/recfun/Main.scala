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
      if (c == 0 || r == 0 || c==r)
        1
      else
        pascal(c, r -1 ) + pascal(c - 1, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
        var count = 0;
        def bal_with_count(chars: List[Char], count: Int): Int = {
          if(!chars.isEmpty && count >= 0) {
            if (chars.head == '(')
              bal_with_count(chars.tail: List[Char], count + 1)
            else if (chars.head == ')')
              bal_with_count(chars.tail: List[Char], count - 1)
            else
              bal_with_count(chars.tail: List[Char], count)
          }
          else
            count
        }

        count =  bal_with_count(chars, count)
        if(count == 0)
          true
         else
          false
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countAux(money: Int, coins: List[Int]): Int ={
        if(money == 0)
          1
        else if (money < 0)
          0
        else {
          var count = 0
          var coins_collect = coins
          while (!coins_collect.isEmpty) {
            count += countAux(money - coins_collect.head, coins_collect)
            coins_collect = coins_collect.tail: List[Int]
          }
          count
        }

      }
      countAux(money, coins)
    }
  }
