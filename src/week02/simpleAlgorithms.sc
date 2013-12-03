package week01

import scala.annotation.tailrec

object simpleAlgorithms {
  //calculates the greatest common divisor by Euclid's algorithm
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)              //> gcd: (a: Int, b: Int)Int

  //calclulates the factorial of a given integer value
  def factorial(n: Int): Int =
    if (n == 0) 1 else n * factorial(n - 1)       //> factorial: (n: Int)Int

  def tailRecursiveFactorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)
    loop(1, n)
  }                                               //> tailRecursiveFactorial: (n: Int)Int

  gcd(14, 21)                                     //> res0: Int = 7

  factorial(5)                                    //> res1: Int = 120

  tailRecursiveFactorial(4)                       //> res2: Int = 24
}