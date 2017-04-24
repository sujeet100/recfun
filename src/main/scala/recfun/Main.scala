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
  def pascal(c: Int, r: Int): Int = if (c == r || c == 0) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceParens(chars: List[Char], stack: List[Char]): Boolean = {
      if (chars.isEmpty) {
        if (stack.isEmpty) true else false
      } else {
        if (chars.head == '(') balanceParens(chars.tail, chars.head :: stack)
        else if (chars.head == ')') if (stack.isEmpty) false else balanceParens(chars.tail, stack.tail)
        else
          balanceParens(chars.tail, stack)
      }

    }

    balanceParens(chars, Nil)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) return 0
    if (money == 0) 1
    else if (coins.isEmpty) 0
    else
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
