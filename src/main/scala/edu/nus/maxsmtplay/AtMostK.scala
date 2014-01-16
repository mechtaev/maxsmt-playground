package edu.nus.maxsmtplay

import com.microsoft.z3._

/**
 * Interface of AtMostK constraint
 */
trait AtMostK extends AtMostOne {

  def atMostK(cs: List[BoolExpr], k:Int)

  override def atMostOne(cs: List[BoolExpr]) = {
    atMostK(cs, 1)
  }

}
