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
      throw new Exception("Hard constraints are not satisfiable")
    }
    // saving soft * aux * (orig * blocks)
    var assumptions = assertAssumptions(soft).map({
      case (s, a) => (s, a, (s, List[Z3AST]()))
    })
    breakable {
      while(true) {
        var blockVars = List[Z3AST]()
        val assumptionsAndSwitches = assumptions.map({case (s, a, ob) => (s, a, ob, z3.mkNot(a))})
        val Some(sat) = solver.checkAssumptions(assumptionsAndSwitches.map(_._4):_*)
        if (sat) break()
        val core = solver.getUnsatCore().toList
        assumptions = assumptionsAndSwitches.map({
          case (soft, aux, (orig, blocks), assumption) =>
            if (core.contains(assumption)) {
              val blockVar = z3.mkFreshBoolConst("b")
              blockVars = blockVar :: blockVars
              val newSoft = z3.mkOr(soft, blockVar)
              val newAux = assertAssumptions(List(newSoft)) match {
                case List((_, a)) => a
              }
              val newBlocks = blockVar :: blocks
              (newSoft, newAux, (orig, newBlocks))
          } else (soft, aux, (orig, blocks))})
        atMostOne(blockVars)
      }
    }

    val model = solver.getModel()
    val result = assumptions.filter({
      case (_, _, (_, blocks)) => {
        val evalList = blocks.map(b => {
          val Some(result) = model.eval(b)
          result.equals(z3.mkTrue)
        })
        !evalList.foldLeft(false)(_ || _)
      }
    })
    //print(z3.modelToString(solver.getModel()))
    result.map({case (_, _, (orig, _)) => orig}) ++ hard
  }

}
