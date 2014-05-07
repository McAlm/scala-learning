package week03

object rationals {
  var x = new Rational(1, 3)                      //> x  : week03.Rational = 1/3
  var y = new Rational(5, 7)                      //> y  : week03.Rational = 5/7
  var z = new Rational(3, 2)                      //> z  : week03.Rational = 3/2


  x.sub(y).sub(z)                                 //> res0: week03.Rational = -79/42
  y + y                                           //> res1: week03.Rational = 10/7
 
 	x < y                                     //> res2: Boolean = true
 	y < x                                     //> res3: Boolean = false
 	
 	x.max(y)                                  //> res4: week03.Rational = 5/7
 	y.max(x)                                  //> res5: week03.Rational = 5/7
 	
 	new Rational(2)                           //> res6: week03.Rational = 2/1
}

class Rational(x: Int, y: Int) {

 def this(x:Int) = this(x, 1);
  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }
  
  private val ggt = gcd(x, y)
 
  def numer = x / ggt
  def denom = y / ggt

  def + (that: Rational): Rational = {
    new Rational(numer * that.denom + denom * that.numer, denom * that.denom)
  }

  def sub(that: Rational): Rational = this + that.neg

  def neg = new Rational(numer * -1, denom);
  
  def < (that:Rational) = numer * that.denom  < that.numer * denom
  
  def max (that:Rational): Rational = if (this < that) that else this

  override def toString = numer + "/" + denom;
}