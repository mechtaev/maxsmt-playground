package edu.nus.maxsmtplay

import z3.scala._

/**
  * Implementation of linear algorithm
  */
trait Linear extends MaxSMT {
  this: Z3 with AtMostK =>

  override def solve(soft: Seq[Z3AST], hard: Seq[Z3AST]): Int = {
    //TODO implement linear iterative algorithm
    0
  }

}
