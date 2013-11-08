package edu.nus.maxsmtplay

import z3.scala._

/**
 * Interface of AtMostK constraint
 */
trait AtMostK {

  def atMostK(cs: Seq[Z3AST], k:Int)

  def atMostOne(cs: Seq[Z3AST]) = {
    atMostK(cs, 1)
  }

}
