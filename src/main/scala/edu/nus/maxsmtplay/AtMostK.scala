package edu.nus.maxsmtplay

import z3.scala._

/**
 * Interface of AtMostK constraint
 */
trait AtMostK extends AtMostOne {

  def atMostK(cs: List[Z3AST], k:Int)

  override def atMostOne(cs: List[Z3AST]) = {
    atMostK(cs, 1)
  }

}
