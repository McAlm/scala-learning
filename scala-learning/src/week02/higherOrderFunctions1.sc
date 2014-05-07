package week01

object higherOrderFunctions1 {

  def sumInts(a: Int, b: Int): Int =
    if (a > b) 0 else a + sumInts(a + 1, b)       //> sumInts: (a: Int, b: Int)Int

  def cube(x: Int) = x * x * x                    //> cube: (x: Int)Int

  def sumCubes(a: Int, b: Int): Int =
    if (a > b) 0 else cube(a) + sumCubes(a + 1, b)//> sumCubes: (a: Int, b: Int)Int

  def factorial(a: Int): Int =
    if (a == 0) 1 else a * factorial(a - 1)       //> factorial: (a: Int)Int

  def sumFactorials(a: Int, b: Int): Int =
    if (a > b) 0 else factorial(a) + sumFactorials(a + 1, b)
                                                  //> sumFactorials: (a: Int, b: Int)Int

  sumInts(1, 5)                                   //> res0: Int = 15
  sumCubes(1, 5)                                  //> res1: Int = 225
  sumFactorials(1, 5)                             //> res2: Int = 153
}