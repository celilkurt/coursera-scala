package week4

abstract class Nat {

  def isZero: Boolean
  def predecessor: Nat
  def successer: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (taht: Nat): Nat

}

object Zero extends Nat{
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new Error("0.predecessor")

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if(that.isZero) this else throw new Error("negative number")
}

class Succ(n: Nat) extends Nat{
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if(that.isZero) this else n - that.predecessor
}
