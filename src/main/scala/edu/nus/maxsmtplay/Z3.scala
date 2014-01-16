package edu.nus.maxsmtplay

import com.microsoft.z3._

trait Z3 {

  var z3: Context = null
  var solver: Solver = null

  def init() = {
    val cfg = new java.util.HashMap[String, String]()
    cfg.put("MODEL", "true")
    z3 = new Context(cfg)
    solver = z3.mkSolver
  }

  def delete() = {
    z3.dispose()
  }

  def assertAssumptions(constraints: List[BoolExpr]): List[(BoolExpr, BoolExpr)] = {
    val pairs = constraints.map(c => (c, z3.mkBoolConst(UniqueName.withPrefix("a"))))
    pairs.map({case (c, a) => solver.add(z3.mkOr(c, a))})
    pairs
  }

}
