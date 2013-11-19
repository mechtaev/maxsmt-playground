
package edu.nus.maxsmtplay

import z3.scala._

abstract class MaxSMT {
  this: Z3 =>

  def solve(soft: List[Z3AST], hard: List[Z3AST]): List[Z3AST]

  def solveFromFile(filename: String): List[Z3AST] = {
    z3.parseSMTLIBFile(filename)
    val hard = z3.getSMTLIBFormulas.toList
    val soft = z3.getSMTLIBAssumptions.toList
    val maxsat = solve(soft, hard)
    maxsat
  }

  def solveFromFileAndGetSoftHard(filename: String): (List[Z3AST], List[Z3AST], List[Z3AST]) = {
    z3.parseSMTLIBFile(filename)
    val hard = z3.getSMTLIBFormulas.toList
    val soft = z3.getSMTLIBAssumptions.toList
    val maxsat = solve(soft, hard)
    (maxsat, soft, hard)
  }



}
