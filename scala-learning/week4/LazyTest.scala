package week4

object LazyTest {

  class X {
    val x = { Thread.sleep(2000); 15 }
    def getX = x
  }

  class Y {
    lazy val y = { Thread.sleep(2000); 13 }
    def getY = y
  }

  def main(args: Array[String]) {
    def x = new X
    def y = new Y
    println("X      " + x.getX)
    println("lazy Y " + y.getY);
    println("X      " + x.getX)
    println("lazy Y " + y.getY);
  }
}