package edu.nus.maxsmtplay

import z3.scala._

trait Verifier {
  this: Z3 =>

  def checkSat(constraints: List[Z3AST]): Boolean = {
    solver.reset()
    solver.assertCnstr(z3.mkAnd(constraints:_*))
    val Some(result) = solver.check()
    result
  }

}
