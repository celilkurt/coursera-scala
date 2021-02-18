package recfun

import scala.collection.mutable.Stack

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    def factorial(n: Int): Int = if (n <= 0) 1 else n * factorial(n - 1)

    factorial(r)/(factorial(c)*factorial(r-c))
  }
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val s = Stack[Char]()

    for(c <- chars){

      c match {
        case '('  => s.push(c)
        case ')' => if(s.isEmpty) s.push(c) else if(s.top == '(') s.pop() else s.push(c)
        case _   =>  // default branch
      }
    }

    s.isEmpty

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def rec(money: Int, coins: List[Int]): Int = {

      (for(i <- 0 until coins.size) yield {
          if(money-coins(i) == 0){
            1
          }else if(money-coins(i) > 0){
            rec(money-coins(i),coins.slice(i,coins.size))
          }else{ // money-coin < 0
            0
          }
        }).sum

    }

    if(money <= 0) {
      0
    }else if(coins.isEmpty){
      0
    }else if(coins.min > money) {
      0
    }else{
      rec(money,coins)
    }

  }
}
