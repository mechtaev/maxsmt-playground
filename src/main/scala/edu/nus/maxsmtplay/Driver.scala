package edu.nus.maxsmtplay

import z3.scala._

/**
 * Entry point
 */
object Driver {

  def main(args: Array[String]): Unit = {
    // FuMalik using Z3 and circuit-style at-most-k constaint
    val solver = new FuMalik with Circuit with Z3

    //TODO parse file and call solver.solve(soft, hard)
  }
  
   def smtlib_maxsat(filename:String, approach:Int) : Int = {
    val ctx = new Z3Context((new Z3Config).setParamValue("MODEL", "true"))
    ctx.parseSMTLIBFile(filename);
    val solver = ctx.mkSolver
    val assumptions = ctx.getSMTLIBAssumptions
    solver.assertCnstr(ctx.mkAnd(assumptions.toSeq:_*))
    println(solver.check());
    return 0;
 }
  

}
