package edu.nus.maxsmtplay

import com.microsoft.z3._

trait Verifier {
  this: Z3 =>

  def checkSat(constraints: List[BoolExpr]): Boolean = {
    solver.reset()
    solver.add(z3.mkAnd(constraints:_*))
    val result = solver.check()
    (result == Status.SATISFIABLE)
  }

}
