package edu.nus.maxsmtplay

import z3.scala._

/**
  * Implementation of Fu-Malik algorithm
  * 
  * For more information on the Fu & Malik procedure:
  * 
  * Z. Fu and S. Malik, On solving the partial MAX-SAT problem, in International 
  * Conference on Theory and Applications of Satisfiability Testing, 2006.
  */
trait FuMalik extends MaxSMT {
  this: AtMostK with Z3 =>

  override def solve(soft: Seq[Z3AST], hard: Seq[Z3AST]): Int = {
    //TODO implement Fu-Malik
    0
  }

}
