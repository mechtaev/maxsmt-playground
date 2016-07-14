package edu.nus.maxsmtplay

import com.microsoft.z3._

trait Z3 {

  var z3: Context = null
  var solver: Solver = null

  def init(timeout: Option[Int]) = {
    val cfg = new java.util.HashMap[String, String]()
    cfg.put("model", "true")
    cfg.put("unsat_core", "true")
    timeout match {
      case Some(time) => cfg.put("timeout", time.toString)
      case _ => ()
    }
    z3 = new Context(cfg)
    solver = z3.mkSolver
  }

  def delete() = {
//    z3.dispose()
  }

  def assertAssumptions(constraints: List[BoolExpr]): List[(BoolExpr, BoolExpr)] = {
    val pairs = constraints.map(c => (c, z3.mkBoolConst(UniqueName.withPrefix("a"))))
    pairs.map({case (c, a) => solver.add(z3.mkOr(c, a))})
    pairs
  }

}
