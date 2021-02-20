package week2

object Rationals extends App {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  println(x - y + z * x)
  println(x < y)
  println(x max y)



}

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)


  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)

  def numer: Int = x / g
  def denom: Int = y / g

  def +(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def *(that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)

  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational): Rational = this + -that

  def < (that: Rational): Boolean = numer * that.denom < that.numer * denom

  def max(that: Rational): Rational = if(that < (this)) this else that


  override def toString: String = numer + "/" + denom


}
