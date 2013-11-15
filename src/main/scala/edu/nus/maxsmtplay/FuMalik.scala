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

  override def solve(soft: List[Z3AST], hard: List[Z3AST]): Int = {
    //hard constraints
    hard.map((c: Z3AST) => solver.assertCnstr(c))
    val Some(sat) = solver.check()
    if (!sat) {
      sys.exit(0)
    }
    var softAndAux = soft.map(c => (c, z3.mkFreshConst("a", z3.mkBoolSort)))
    //assert soft and aux:
    val toAssert = softAndAux.map({case (c, a) => z3.mkOr(c, a)})
    println(toAssert.size)
    toAssert.map(solver.assertCnstr)
    //solver.assertCnstr(z3.mkAnd(toAssert:_*))
    var count = 0
    breakable {
      while(true) {
        var blockVars = List[Z3AST]()
        val softAndAuxAndAssumptions = softAndAux.map({case (s, a) => (s, a, z3.mkNot(a))})
        val Some(sat) = solver.checkAssumptions(softAndAuxAndAssumptions.map(_._3):_*)
        if (sat) break()
        val core = solver.getUnsatCore().toList
        //core.map({ ast => println(z3.astToString(ast))})
        softAndAux = softAndAuxAndAssumptions.map({case (soft, aux, assumption) =>
          if (core.contains(assumption)) {
            val blockVar = z3.mkFreshBoolConst("b")
            blockVars = blockVar :: blockVars
            val newAux = z3.mkFreshBoolConst("n")
            val newSoft = z3.mkOr(soft, blockVar)
            solver.assertCnstr(z3.mkOr(newSoft, newAux))
            (newSoft, newAux)
          } else (soft, aux)})
        atMostOne(blockVars)
        count = count + 1
      }
    }

    println(z3.modelToString(solver.getModel()))

    //TODO return somethins here
    soft.size - count
  }

}
