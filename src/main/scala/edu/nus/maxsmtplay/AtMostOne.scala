package edu.nus.maxsmtplay

import com.microsoft.z3._

trait AtMostOne {
  
  def atMostOne(cs: List[BoolExpr])

}
