package edu.nus.maxsmtplay

import com.microsoft.z3._

import scala.util.control.Breaks._

/**
  * Implementation of Fu-Malik algorithm
  * 
  * For more information on the Fu & Malik procedure:
  * 
  * Z. Fu and S. Malik, On solving the partial MAX-SAT problem, in International 
  * Conference on Theory and Applications of Satisfiability Testing, 2006.
  */
abstract class FuMalik(bound: Option[Int]) extends MaxSMT with Printer {
  this: AtMostOne with Z3 =>

  var lastCores: List[Int] = Nil

  override def solveAndGetModel(soft: List[BoolExpr], hard: List[BoolExpr]): Option[(List[BoolExpr], Model)] = {
    lastCores = Nil
    //hard constraints
    hard.map((c: BoolExpr) => solver.add(c))
    // System.err.println("SOFT: " + soft.size)
    
    //FIXME should I check formula before solving?
    // val Some(sat) = solver.check()
    // if (!sat) {
    //   throw new Exception("Hard constraints are not satisfiable")
    // }

    // saving (soft * aux) * (orig * blocks)
    var assumptions = assertAssumptions(soft).map({
      case (s, a) => ((s, a), (s, List[BoolExpr]()))
    })
    var count = 0
    breakable {
      while(true) {
        var blockVars = List[BoolExpr]()
        val assumptionsAndSwitches = 
          assumptions.map({case ((s, a), ob) => ((s, a), ob, z3.mkNot(a))})
        assumptions.map({case ((s, a), _) => solver.add(z3.mkOr(s, a))})
        val checkResult = solver.check(assumptionsAndSwitches.map(_._3):_*)
        val sat = (checkResult == Status.SATISFIABLE)
        if (sat) break()
        count = count + 1
        bound match {
          case Some(v) if count > v => return None
          case _ => ()
        }
        val core = solver.getUnsatCore().toList
        lastCores = lastCores ++ List(core.size) //saving unsat-core size

        var coreLog = List[BoolExpr]()
        assumptions = assumptionsAndSwitches.map({
          case ((soft, aux), (orig, oldBlocks), switch) => {
            if (core.size == 0 || core.contains(switch)) {
              coreLog = soft :: coreLog
              val blockVar = z3.mkBoolConst(UniqueName.withPrefix("b"))
              blockVars = blockVar :: blockVars
              val newSoft = z3.mkOr(soft, blockVar)
              val newAux = z3.mkBoolConst(UniqueName.withPrefix("a"))
              val newBlocks = blockVar :: oldBlocks
              ((newSoft, newAux), (orig, newBlocks))
            } else {
              ((soft, aux), (orig, oldBlocks))
            }
          }
        })

        //writeLog("fumalik-core", coreLog.map({c => c.toString + "\n"}).reduceLeft(_ + _))

        atMostOne(blockVars)
      }
    }
    val model = solver.getModel()
    val result = assumptions.filter({
      case (_, (_, blocks)) => {
        val evalList = blocks.map(b => {
          //FIXME what does this true mean?
          val result = model.eval(b, true)
          result.equals(z3.mkFalse)
        })
        evalList.foldLeft(true)(_ && _)
      }
    })
    writeLog("fumalik-model", solver.getModel().toString())
    Some((result.map({case (_, (orig, _)) => orig}) ++ hard, model))
  }

}
