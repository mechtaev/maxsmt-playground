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
abstract class FuMalik extends MaxSMT with Printer {
  this: AtMostOne with Z3 =>

  override def solve(soft: List[BoolExpr], hard: List[BoolExpr]): List[BoolExpr] = {
    //hard constraints
    hard.map((c: BoolExpr) => solver.add(c))
    // val Some(sat) = solver.check()
    // if (!sat) {
    //   throw new Exception("Hard constraints are not satisfiable")
    // }
    // saving (soft * aux) * (orig * blocks)
    var assumptions = assertAssumptions(soft).map({
      case (s, a) => ((s, a), (s, List[BoolExpr]()))
    })
    var iter = 0
    breakable {
      while(true) {
        println("Iter " + iter)
        var blockVars = List[BoolExpr]()
        val assumptionsAndSwitches = 
          assumptions.map({case ((s, a), ob) => ((s, a), ob, z3.mkNot(a))})
        //printConstraints("Soft", assumptions.map({case ((s, a), ob) => z3.mkOr(a, s)}))
        //why I should do it here?
        assumptions.map({case ((s, a), _) => solver.add(z3.mkOr(s, a))})
        val checkResult = solver.check(assumptionsAndSwitches.map(_._3):_*)
        val sat = (checkResult == Status.SATISFIABLE)
        if (sat) break()
        iter += 1
        val core = solver.getUnsatCore().toList
        // println("Core size " + core.size)
        // println("Core: ")
        // println(core)
        var coreLog = List[BoolExpr]()
        assumptions = assumptionsAndSwitches.map({
          case ((soft, aux), (orig, oldBlocks), switch) => {
            if (core.contains(switch)) {
              coreLog = soft :: coreLog
              val blockVar = z3.mkBoolConst(UniqueName.withPrefix("b"))
              blockVars = blockVar :: blockVars
              val newSoft = z3.mkOr(soft, blockVar)
              val newAux = z3.mkBoolConst(UniqueName.withPrefix("a"))
              //solver.add(z3.mkOr(newSoft, newAux))
              //val newAssumption = assertAssumptions(List(newSoft)).head
              val newBlocks = blockVar :: oldBlocks
              ((newSoft, newAux), (orig, newBlocks))
            } else {
              //solver.add(z3.mkOr(soft, aux))
              ((soft, aux), (orig, oldBlocks))
            }
          }
        })

        writeLog("fumalik-core", coreLog.map({c => c.toString + "\n"}).reduceLeft(_ + _))

        //println("After assumptions: " + solver.getAssertions().size)

        atMostOne(blockVars)

        assumptions.map({case ((s, a), _) => solver.add(z3.mkOr(s, a))})

        //println("After blockVars: " + solver.getAssertions().size)


        //val x = z3.mkFreshBoolConst("x")
        //solver.add(z3.mkFreshBoolConst("x"))

        // for( a <- 1 to 100){
        //    val x = z3.mkFreshBoolConst("x")
        //   solver.add(x)
        // }
      }
    }
    //assert(solver.isModelAvailable)
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
    result.map({case (_, (orig, _)) => orig}) ++ hard
  }

}
