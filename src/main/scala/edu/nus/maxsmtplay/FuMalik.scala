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
abstract class FuMalik extends MaxSMT with Printer {
  this: AtMostOne with Z3 =>

  override def solve(soft: List[Z3AST], hard: List[Z3AST]): List[Z3AST] = {
    //hard constraints
    hard.map((c: Z3AST) => solver.assertCnstr(c))
    // val Some(sat) = solver.check()
    // if (!sat) {
    //   throw new Exception("Hard constraints are not satisfiable")
    // }
    // saving (soft * aux) * (orig * blocks)
    var assumptions = assertAssumptions(soft).map({
      case (s, a) => ((s, a), (s, List[Z3AST]()))
    })
    var iter = 0
    breakable {
      while(true) {
        //println("Iter " + iter)
        var blockVars = List[Z3AST]()
        val assumptionsAndSwitches = 
          assumptions.map({case ((s, a), ob) => ((s, a), ob, z3.mkNot(a))})
        //printConstraints("Soft", assumptions.map({case ((s, a), ob) => z3.mkOr(a, s)}))
        //why I should do it here?
        assumptions.map({case ((s, a), _) => solver.assertCnstr(z3.mkOr(s, a))})
        val Some(sat) = solver.checkAssumptions(assumptionsAndSwitches.map(_._3):_*)
        if (sat) break()
        iter += 1
        val core = solver.getUnsatCore().toList
        // println("Core size " + core.size)
        // println("Core: ")
        // println(core)
        var coreLog = List[Z3AST]()
        assumptions = assumptionsAndSwitches.map({
          case ((soft, aux), (orig, oldBlocks), switch) => {
            if (core.contains(switch)) {
              coreLog = soft :: coreLog
              val blockVar = z3.mkFreshBoolConst("b")
              blockVars = blockVar :: blockVars
              val newSoft = z3.mkOr(soft, blockVar)
              val newAux = z3.mkFreshBoolConst("a")
              //solver.assertCnstr(z3.mkOr(newSoft, newAux))
              //val newAssumption = assertAssumptions(List(newSoft)).head
              val newBlocks = blockVar :: oldBlocks
              ((newSoft, newAux), (orig, newBlocks))
            } else {
              //solver.assertCnstr(z3.mkOr(soft, aux))
              ((soft, aux), (orig, oldBlocks))
            }
          }
        })
        writeLog("fumalik-core", coreLog.map({c => c.toString + "\n"}).reduceLeft(_ + _))

        atMostOne(blockVars)
      }
    }
    assert(solver.isModelAvailable)
    val model = solver.getModel()
    val result = assumptions.filter({
      case (_, (_, blocks)) => {
        val evalList = blocks.map(b => {
          val Some(result) = model.eval(b)
          result.equals(z3.mkFalse)
        })
        evalList.foldLeft(true)(_ && _)
      }
    })
    writeLog("fumalik-model", z3.modelToString(solver.getModel()))
    result.map({case (_, (orig, _)) => orig}) ++ hard
  }

}
