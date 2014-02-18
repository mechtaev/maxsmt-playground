package edu.nus.maxsmtplay

import com.microsoft.z3._

trait SolverTestUtils extends MaxSMT {
  this: Z3 =>

  def solveFromFile(filename: String): List[BoolExpr] = {
    println("parsing...")
    z3.parseSMTLIBFile(filename, null, null, null, null)
    println("extracting constraints...")
    val hard = z3.getSMTLIBFormulas.toList
    val soft = z3.getSMTLIBAssumptions.toList
    println("solving...")
    val Some(maxsat) = solve(soft, hard)
    maxsat
  }

  def solveFromFileAndGetSoftHard(filename: String): (List[BoolExpr], List[BoolExpr], List[BoolExpr]) = {
    z3.parseSMTLIBFile(filename, null, null, null, null)
    val hard = z3.getSMTLIBFormulas().toList
    val soft = z3.getSMTLIBAssumptions().toList
    val Some(maxsat) = solve(soft, hard)
    (maxsat, soft, hard)
  }

}
