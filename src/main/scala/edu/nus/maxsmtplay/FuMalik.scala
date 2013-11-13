package edu.nus.maxsmtplay

import z3.scala._
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
  this: AtMostK with Z3 =>

  override def solve(soft: Seq[Z3AST], hard: Seq[Z3AST]): Int = {

    // here we read from file and select solf and hard constraints. Probably it must be in Driver.
    /*
    z3.parseSMTLIBFile(file)
    val hard = z3.getSMTLIBFormulas
    val soft = z3.getSMTLIBAssumptions
     */
    var softAndAux = soft.map(c => (c, z3.mkFreshBoolConst("k")))
     

    hard.map((c: Z3AST) => solver.assertCnstr(c))
    val Some(sat) = solver.check()
    if (!sat) {
      sys.exit(0)
    }

    //assert soft and aux:
    softAndAux.map((c: (Z3AST, Z3AST)) => solver.assertCnstr(z3.mkBVOr(c._1, c._2)))

    breakable {
      while(true) {
        var blockVars = List[Z3AST]()
        val assumptions = softAndAux.map(_._2).map(z3.mkNot).toSeq
        val Some(sat) = solver.checkAssumptions(assumptions:_*)
        if (sat) break()
        val core = solver.getUnsatCore().toSeq
        softAndAux = softAndAux.map({case (soft, aux) =>
          if (core.contains(aux)) {
            val blockVar = z3.mkFreshBoolConst("k")
            blockVars = blockVar :: blockVars
            val newAux = z3.mkFreshBoolConst("k")
            val newSoft = z3.mkBVOr(soft, blockVar)
            solver.assertCnstr(z3.mkBVOr(newSoft, newAux))
            (newSoft, newAux)
          } else (soft, aux)})
        atMostOne(blockVars)
      }
    }

    println(z3.modelToString(solver.getModel()))

    //TODO return somethins here
    0
  }

}
