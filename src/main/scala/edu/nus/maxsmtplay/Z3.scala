package edu.nus.maxsmtplay

import z3.scala._

trait Z3 {

  var z3: Z3Context = null
  var solver: Z3Solver = null

  def init() = {
    z3 = new Z3Context(new Z3Config("MODEL" -> true))
    solver = z3.mkSolver
  }

  def delete() = {
    z3.delete()
  }

  def assertAssumptions(constraints: List[Z3AST]): List[(Z3AST, Z3AST)] = {
    val pairs = constraints.map(c => (c, z3.mkFreshBoolConst("a")))
    pairs.map({case (c, a) => solver.assertCnstr(z3.mkOr(c, a))})
    pairs
  }

}
