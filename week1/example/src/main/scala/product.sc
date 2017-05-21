object exercise {
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  def general(f: Int => Int, zero: Int, op: (Int, Int) => Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else op(f(a), general(f, zero, op)(a + 1, b))
  }

  def prod(f: Int => Int)(a: Int, b: Int): Int = {
    general(f, 1, (x, y) => x * y)(a, b)
  }

  def factorial(a: Int): Int = {
    prod(x => x) (1, a)
  }

  product(x => x * x) (3, 4)
  factorial(5)

}

