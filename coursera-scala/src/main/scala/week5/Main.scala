package week5

object Main extends App {

  println(sum(List(5,23,6,5)))
  println(product(List(5,23,6,5)))



  def removeAt[T](n: Int, xs: List[T]): List[T] =
    if(n == 0 && xs.nonEmpty) xs.tail
    else if(xs.nonEmpty) xs.head :: removeAt(n-1, xs.tail)
    else throw new Error("n is too big")


  def sum(list: List[Int]): Int =
    list match {
      case Nil => 0
      case (head :: tail) => head + sum(tail)
    }

  def product(list: List[Int]): Int =
    list match {
      case Nil => 1
      case (head :: tail) => head * product(tail)
    }

}
