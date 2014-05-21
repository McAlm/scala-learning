package week4

import scala.annotation.tailrec

abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

class Succ(n: Nat) extends Nat {

  def isZero: Boolean = false

  def predecessor: Nat = n

  def +(that: Nat) = new Succ(n + that)

  def -(that: Nat) = if(that.isZero) this else n - that.predecessor
}

object Null extends Nat {
  def isZero: Boolean = true

  def predecessor: Nat = throw new Exception("Zero has no predecessor for natural numbers!");

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = if (that.isZero) Null else throw new Exception("Cannot subtract from Null for natural numbers!");
}

