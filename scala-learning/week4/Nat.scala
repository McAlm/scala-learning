package week4

import scala.annotation.tailrec

abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat
  def successor: Nat
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

class Succ(n: Nat) extends Nat {

  def isZero: Boolean = false

  def predecessor: Nat = this - (new Succ(Null))

  def successor: Nat = this + (new Succ(Null))

  def +(that: Nat) = {
    @tailrec
    def loop(that: Nat, result: Nat): Nat = {
      if (that.isZero) result
      loop(that.predecessor, result.successor)
    }
    loop(that, this)
  }

  def -(that: Nat) = {
    @tailrec
    def loop(that: Nat, result: Nat): Nat = {
      if (that.isZero) result
      loop(that.predecessor, result.predecessor)
    }
    loop(that, this)
  }
}

object Null extends Nat {
  def isZero: Boolean = true

  def predecessor: Nat = throw new Exception("Zero has no predecessor for natural numbers!");

  def successor: Nat = new Succ(Null)

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = if (that.isZero) Null else throw new Exception("Cannot subtract from Null for natural numbers!");
}

