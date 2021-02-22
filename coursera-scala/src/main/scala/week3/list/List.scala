package week3.list

import java.util.NoSuchElementException

trait List[T] {
  def isEmpty: Boolean
  def head:    T
  def tail:    List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head:    Nothing = throw new NoSuchElementException("Nil.head")
  def tail:    Nothing = throw new NoSuchElementException("Nil.tail")
}

object Main extends App {

  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

  println(nth(3,list))

  def nth[T](n: Int, list: List[T]): T =

    if(list.isEmpty) throw new NoSuchElementException()
    else if(n == 0) list.head
    else nth(n - 1, list.tail)

}