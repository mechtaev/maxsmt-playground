package edu.nus.maxsmtplay

import z3.scala._

/**
  * Implementation of linear algorithm
  */
trait Linear extends MaxSMT {
  this: Z3 with AtMostK =>

  override def solve(soft: List[Z3AST], hard: List[Z3AST]): Int = {
    //TODO implement linear iterative algorithm
    //asserthardconstraints
    val is_sat = solver.context.check()
    if(is_sat == Some(false)){
      -1
    } 
    0
  }

}
