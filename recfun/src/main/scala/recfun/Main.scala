package recfun

object Main {
  def main(args: Array[String]) {
    //    println("Pascal's Triangle")
    //    for (row <- 0 to 10) {
    //      for (col <- 0 to row)
    //        print(pascal(col, row) + " ")
    //      println()
    //    }

    println("---------------------------")
    println(sumInts(1, 3))
    println(sumCubes(1, 3))
    println(sumFactorials(1, 3))
  }

  def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sum(f, a + 1, b)

  def sumInts(a: Int, b: Int) = sum(id, a, b)
  def sumCubes(a: Int, b: Int) = sum(cube, a, b)
  def sumFactorials(a: Int, b: Int) = sum(fact, a, b)

  def fact(x: Int): Int = if (x == 0) 1 else fact(x - 1)
  def id(x: Int): Int = x
  def cube(x: Int): Int = x * x * x

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceWithCounter(chars: List[Char], openParen: Int): Boolean = {
      if (chars.isEmpty) openParen == 0
      else {
        val num =
          if (chars.head == '(') openParen + 1
          else if (chars.head == ')') openParen - 1
          else openParen
        if (num >= 0) balanceWithCounter(chars.tail, num)
        else false
      }
    }
    balanceWithCounter(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
}
