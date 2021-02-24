package week4

trait Expr {
  def isNumber: Boolean
  def isSum: Boolean
  def isProd: Boolean
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr
}

class Number(n: Int) extends Expr {
  override def isNumber: Boolean = true

  override def isSum: Boolean = false

  override def numValue: Int = n

  override def leftOp: Expr = throw new Error("Number.leftOp")

  override def rightOp: Expr = throw new Error("Number.rightOp")

  override def isProd: Boolean = false
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  override def isNumber: Boolean = false

  override def isSum: Boolean = true

  override def numValue: Int = throw new Error("Sum.numValue")

  override def leftOp: Expr = e1

  override def rightOp: Expr = e2

  override def isProd: Boolean = false
}

class Prod(e1: Expr, e2: Expr) extends Expr{

  override def isNumber: Boolean = false

  override def isSum: Boolean = true

  override def numValue: Int = throw new Error("Prod.numValue")

  override def leftOp: Expr = e1

  override def rightOp: Expr = e2

  override def isProd: Boolean = true
}


object Main extends App {

  def eval(e: Expr): Int = {
    if(e.isNumber) e.numValue
    else if(e.isSum) eval(e.leftOp) + eval(e.rightOp)
    else throw new Error("Unknown expression " + e)
  }

  def prod(e: Expr): Int = {
    if(e.isNumber) e.numValue
    else if(e.isSum) eval(e.leftOp) * eval(e.rightOp)
    else throw new Error("Unknown expression " + e)
  }

  val expr = new Prod(new Prod(new Number(3),new Prod(new Number(5), new Number(6))),new Number(10))
  println(prod(expr))

}
