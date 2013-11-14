package edu.nus.maxsmtplay

trait FileDriver {
  this: Z3 with MaxSMT =>

  def solveFromFile(filename: String): Int = {
    init()
    z3.parseSMTLIBFile(filename)
    val hard = z3.getSMTLIBFormulas
    val soft = z3.getSMTLIBAssumptions
    val unsat = solve(soft.toList, hard.toList)
    delete()
    unsat
  }

}