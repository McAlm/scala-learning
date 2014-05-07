package week03

object intsets {
 val t1 = new NonEmpty(3, Empty, Empty)           //> t1  : week03.NonEmpty = {.3.}
 val t2 = t1 incl 4                               //> t2  : week03.IntSet = {.3{.4.}}
 val t3 = t2 incl 1                               //> t3  : week03.IntSet = {{.1.}3{.4.}}
 val t4 = t1 incl 2                               //> t4  : week03.IntSet = {{.2.}3.}
 
 
 val t5 = t3 union t4                             //> t5  : week03.IntSet = {{{.1.}2.}3{.4.}}
}

abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean
  
  def union(other:IntSet):IntSet
}

object Empty extends IntSet {

  def contains(x: Int) = false
  def incl(x: Int) = new NonEmpty(x, Empty, Empty)
  def union(other:IntSet) = other
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

	def union(other:IntSet):IntSet = ((left union right) union other) incl elem
	
	override def toString =  "{" + left  + elem + right + "}"
}