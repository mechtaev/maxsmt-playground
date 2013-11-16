package edu.nus.maxsmtplay

import z3.scala._
import z3.scala.dsl._

import scala.util.control.Breaks._

/**
  * Implementation of Fu-Malik algorithm
  * 
  * For more information on the Fu & Malik procedure:
  * 
  * Z. Fu and S. Malik, On solving the partial MAX-SAT problem, in International 
  * Conference on Theory and Applications of Satisfiability Testing, 2006.
  */
trait FuMalik extends MaxSMT {
  this: AtMostOne with Z3 =>

  override def solve(soft: List[Z3AST], hard: List[Z3AST]): List[Z3AST] = {
    //hard constraints
    hard.map((c: Z3AST) => solver.assertCnstr(c))
    val Some(sat) = solver.check()
    if (!sat) {
      sys.exit(0)
    }
    var assumptions = assertAssumptions(soft)
    var count = 0
    breakable {
      while(true) {
        var blockVars = List[Z3AST]()
        val assumptionsAndSwitches = assumptions.map({case (s, a) => (s, a, z3.mkNot(a))})
        val Some(sat) = solver.checkAssumptions(assumptionsAndSwitches.map(_._3):_*)
        if (sat) break()
        val core = solver.getUnsatCore().toList
        assumptions = assumptionsAndSwitches.map({case (soft, aux, assumption) =>
          if (core.contains(assumption)) {
            val blockVar = z3.mkFreshBoolConst("b")
            blockVars = blockVar :: blockVars
            val newSoft = z3.mkOr(soft, blockVar)
            assertAssumptions(List(newSoft)).head
          } else (soft, aux)})
        atMostOne(blockVars)
        count = count + 1
      }
    }




    println("FuMalik return: " + (soft.size - count))
    print(z3.modelToString(solver.getModel()))
    //TODO return constraints
    return List()
  }

}
