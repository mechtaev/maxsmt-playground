package edu.nus.maxsmtplay

import com.microsoft.z3._

abstract class MaxSMT {
  this: Z3 =>

  def solve(soft: List[BoolExpr], hard: List[BoolExpr]): Option[List[BoolExpr]] = {
    solveAndGetModel(soft, hard) match {
      case None => None
      case Some((clauses, _)) => Some(clauses)
    }
  }

  def solveAndGetModel(soft: List[BoolExpr], hard: List[BoolExpr]): Option[(List[BoolExpr], Model)]

}
