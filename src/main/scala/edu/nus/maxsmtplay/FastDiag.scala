package edu.nus.maxsmtplay

import z3.scala._

/**
  * Implementation of Fast-Diag algorithm
  *
  * For more information on the Fast-Diag procedure:
  *
  * Felfernig, A. and Schubert, M. and Zehentner, C. An Efficient Diagnosis
  * Algorithm for Inconsistent Constraint Sets, Artif. Intell. Eng. Des. Anal.
  * Manuf. P. 53-62. 2012.
  */
trait FastDiag extends MaxSMT {
  this: Z3 =>

  override def solve(soft: List[Z3AST], hard: List[Z3AST]): List[Z3AST] = {
    val softAssumptions = assertAssumptions(soft)
    val softAux = softAssumptions.map(_._2)
    val hardAssumptions = assertAssumptions(hard)
    val hardAux = hardAssumptions.map(_._2)
    val mcs = fastDiag(softAux ++ hardAux, softAux, false)
    softAssumptions.filter({case (s, a) => !mcs.contains(a)}).map(_._1)
  }

  def fastDiag(r: List[Z3AST], t: List[Z3AST], hasD: Boolean): List[Z3AST] = {
    val Some(sat) = solver.checkAssumptions(r.map(z3.mkNot):_*)
    if (hasD && sat) 
      return List()
    if (t.size == 1)
      return t
    val m = t.size / 2
    val (tLeft, tRight) = (t.slice(0, m-1), t.slice(m, t.size))
    val rMinusTLeft = r.filter((ast: Z3AST) => !tLeft.contains(ast))
    val dRight = fastDiag(rMinusTLeft, tRight, tLeft.size != 0)
    val rMinusDRight = r.filter((ast: Z3AST) => !dRight.contains(ast))
    val dLeft = fastDiag(rMinusDRight, tLeft, dRight.size != 0)
    dLeft ++ dRight
  }

}
