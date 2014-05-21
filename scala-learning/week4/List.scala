package week4

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(n: Int): T
}

class Nil[T] extends List[T]{
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  def nth(n:Int) :Nothing = throw new IndexOutOfBoundsException("No element at position " + n)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty:Boolean = false
  
  def nth(n:Int): T = {
    def nth(m:Int, x: List[T]): T = {
      if (x.isEmpty) throw new IndexOutOfBoundsException("No element at position " + n) 
      else if (m == 0) x.head else nth(m-1, x.tail)
    }
    
    nth(n, this)
  }
}

object excercise extends App{
  val list = new Cons[Int](1, new Cons(2, new Cons(3,new Nil[Int]())))
  for(i<- 0 to 2){
	  println(list.nth(i))
  }
  //throws Exception
  println(list.nth(3))
}

object List{
  //List(1, 2) = apply(1,2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil));
  
  def apply[T]() = new Nil;
  
}