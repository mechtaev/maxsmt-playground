package edu.nus.maxsmtplay

import z3.scala._

/**
 * Interface of AtMostK constraint
 */
trait Circuit extends AtMostK {
  this: Z3 =>

  override def atMostK(cs: Seq[Z3AST], k:Int) = {
    //TODO
  }

}






