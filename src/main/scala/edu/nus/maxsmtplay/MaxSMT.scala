package edu.nus.maxsmtplay

import com.microsoft.z3._

abstract class MaxSMT {
  this: Z3 =>

  def solve(soft: List[BoolExpr], hard: List[BoolExpr]): List[BoolExpr]

  def solveAndGetModel(soft: List[BoolExpr], hard: List[BoolExpr]): (List[BoolExpr], Model)

}
