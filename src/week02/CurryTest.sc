package week01;

object CurryTest {

  def filter(xs: List[Int], p: Int => Boolean): List[Int] =
    {
      if (xs.isEmpty) xs
      else if (p(xs.head)) xs.head :: filter(xs.tail, p)
      else filter(xs.tail, p)
    }                                             //> filter: (xs: List[Int], p: Int => Boolean)List[Int]

  def modN(n: Int)(x: Int) = {
    ((x % n) == 0)
  }                                               //> modN: (n: Int)(x: Int)Boolean

  val nums = List(1, 2, 3, 4, 5, 6, 7, 8)         //> nums  : List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8)
  println(filter(nums, modN(1)))                  //> List(1, 2, 3, 4, 5, 6, 7, 8)
  println(filter(nums, modN(2)))                  //> List(2, 4, 6, 8)
  println(filter(nums, modN(3)))                  //> List(3, 6)
  
  
  val list = new java.util.ArrayList();           //> list  : java.util.ArrayList[Nothing] = []

}