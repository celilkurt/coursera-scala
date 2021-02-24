package week4.expressions


trait Expr {
  def eval: Int
}

class Number(n: Int) extends Expr{
  override def eval: Int = n
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  override def eval: Int = e1.eval + e2.eval
}

class Prod(e1: Expr, e2: Expr) extends Expr {
  override def eval: Int = e1.eval * e2.eval
}

class Sub(e1: Expr, e2: Expr) extends Expr {
  override def eval: Int = e1.eval - e2.eval
}

object main extends App {

  val expr = new Prod( new Sum(new Number(2),new Prod(new Number(4), new Number(6))), new Number(2))

  println(expr.eval)

}
