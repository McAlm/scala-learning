package week01

object sqrt {

  def abs(x: Double) =
    if (x < 0) -x else x                          //> abs: (x: Double)Double

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double): Boolean =
      abs(guess * guess - x) / x < 0.000001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double

  sqrt(2.0)                                       //> res0: Double = 1.4142135623746899
  sqrt(4)                                         //> res1: Double = 2.0000000929222947
  sqrt(9)                                         //> res2: Double = 3.000000001396984
  sqrt(1e-6)                                      //> res3: Double = 0.0010000001533016628
  sqrt(1e19)                                      //> res4: Double = 3.162277662477113E9

}