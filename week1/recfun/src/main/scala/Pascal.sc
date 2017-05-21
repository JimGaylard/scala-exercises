import scala.annotation.tailrec

def pascal(c: Int, r: Int): Int = {

  factorial(r) / (factorial(c) * factorial(r - c))
}

def factorial(x: Int): Int = {
  loop(1, x)
}

@tailrec
def loop(acc: Int, n: Int): Int = {
  if (n == 0) acc
  else loop(acc * n, n-1)
}

loop(1, 4)
