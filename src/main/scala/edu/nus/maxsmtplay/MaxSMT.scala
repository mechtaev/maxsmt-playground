
package edu.nus.maxsmtplay

import com.microsoft.z3._

abstract class MaxSMT {
  this: Z3 =>

  def solve(soft: List[BoolExpr], hard: List[BoolExpr]): List[BoolExpr]

  def solveFromFile(filename: String): List[BoolExpr] = {
    println("parsing...")
    z3.parseSMTLIBFile(filename, null, null, null, null)
    println("extracting constraints...")
    val hard = z3.getSMTLIBFormulas.toList
    val soft = z3.getSMTLIBAssumptions.toList
    println("solving...")
    val maxsat = solve(soft, hard)
    maxsat
  }

  def solveFromFileAndGetSoftHard(filename: String): (List[BoolExpr], List[BoolExpr], List[BoolExpr]) = {
    z3.parseSMTLIBFile(filename, null, null, null, null)
    val hard = z3.getSMTLIBFormulas().toList
    val soft = z3.getSMTLIBAssumptions().toList
    val maxsat = solve(soft, hard)
    (maxsat, soft, hard)
  }

}
