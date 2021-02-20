package week2

object Main {

  def main(args: Array[String]): Unit = {

    println(sum(pow,0,5))

  }

  def pow(n: Int): Int = n*n


  // Lecture 2.1 - Higher-Order Functions
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a+1, acc+f(a))
    }
    loop(a, 0)
  }

}
