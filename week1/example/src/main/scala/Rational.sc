class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non-zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  def numer: Int = x
  def denom: Int = y


  def add(that: Rational): Rational = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }

  def neg: Rational = {
    new Rational(-numer, denom)
  }

  def sub(that: Rational): Rational = {
    add(that.neg)
  }

  def max(that: Rational) = if (this.less(that)) that else this

  def less(that: Rational): Boolean = numer * that.denom < that.numer * denom

  override def toString: String = {
    val g = gcd(numer, denom)
    numer / g + "/" + denom / g
  }
}

object rationals {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  val a = new Rational(2)
  x.add(y)
  x.sub(y).sub(z)
  y.add(y)
  x.less(y)
  x.max(y)
}
