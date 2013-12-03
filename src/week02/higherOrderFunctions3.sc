package week01

object higherOrderFunctions3 {

  def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum(f, a + 1, b)     //> sum: (f: Int => Int, a: Int, b: Int)Int

  def tailRecursiveSum(f: Int => Int, a: Int, b: Int) = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }                                               //> tailRecursiveSum: (f: Int => Int, a: Int, b: Int)Int

  def factorial(a: Int): Int =
    if (a == 0) 1 else a * factorial(a - 1)       //> factorial: (a: Int)Int

  def sumInts(a: Int, b: Int) = tailRecursiveSum(x => x, a, b)
                                                  //> sumInts: (a: Int, b: Int)Int
  def sumCubes(a: Int, b: Int) = tailRecursiveSum(x => x * x * x, a, b)
                                                  //> sumCubes: (a: Int, b: Int)Int
  def sumFactorials(a: Int, b: Int) = tailRecursiveSum(factorial, a, b)
                                                  //> sumFactorials: (a: Int, b: Int)Int

  sumInts(1, 5)                                   //> res0: Int = 15
  sumCubes(1, 5)                                  //> res1: Int = 225
  sumFactorials(1, 5)                             //> res2: Int = 153
}