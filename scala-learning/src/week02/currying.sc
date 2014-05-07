package week01

object currying {

  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0 else f(a) + sumF(a + 1, b)
    sumF
  }                                               //> sum: (f: Int => Int)(Int, Int) => Int

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int

  product(x => x * x)(3, 5)                       //> res0: Int = 3600

  def factorial(a: Int): Int = product(x => x)(1, a)
                                                  //> factorial: (a: Int)Int
  factorial(4)                                    //> res1: Int = 24

  sum(x => x * x * x)(1, 5)                       //> res2: Int = 225
  sum(x => x)(1, 5)                               //> res3: Int = 15
  sum(x => x * x)(1, 5)                           //> res4: Int = 55
}