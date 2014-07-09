package edu.nus.maxsmtplay

import com.microsoft.z3._

import scala.util.control.Breaks._

/**
  * Checking satisfiability of hard constraints
  */
abstract class Sat() extends MaxSMT with Printer {
  this: Z3 =>

  override def solveAndGetModel(soft: List[BoolExpr], hard: List[BoolExpr]): Option[(List[BoolExpr], Model)] = {
    hard.map((c: BoolExpr) => solver.add(c))
    
    val checkResult = solver.check()
    val sat = (checkResult == Status.SATISFIABLE)
    if (!sat) {
      return None
    }
    val model = solver.getModel()
    Some((Nil, model))
  }

}
