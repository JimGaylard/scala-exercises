def sqrt(x: Double) = {
  def sqrtIter(guess: Double): Double = {
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))
  }

  def isGoodEnough(guess: Double): Boolean = {
    Math.abs(guess * guess - x) < 0.001
  }

  def improve(guess: Double): Double = {
    (guess + x / guess) / 2
  }

  sqrtIter(3.0)
}

sqrt(16)
