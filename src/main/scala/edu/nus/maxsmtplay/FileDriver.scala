package edu.nus.maxsmtplay

import z3.scala._

trait FileDriver {
  this: Z3 with MaxSMT =>

  def solveFromFile(filename: String): List[Z3AST] = {
    z3.parseSMTLIBFile(filename)
    val hard = z3.getSMTLIBFormulas
    val soft = z3.getSMTLIBAssumptions
    val maxsat = solve(soft.toList, hard.toList)
    maxsat
  }

  def printConstraints(constraints: List[Z3AST]) = {
    println("" + constraints.size + " constraints:")
    constraints.map(printlnAST)
  }

}
