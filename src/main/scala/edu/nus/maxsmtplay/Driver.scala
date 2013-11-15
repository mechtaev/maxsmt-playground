package edu.nus.maxsmtplay

import z3.scala._

/**
 * Entry point
 */
object Driver {

  def main(args: Array[String]): Unit = {
    // FuMalik using Z3 and circuit-style at-most-k constaint
   /* val solver = new FileDriver with FuMalik with Circuit with Z3
    val file = "/home/shinhwei/workspace/z3/maxsmt-playground/src/main/scala/edu/nus/maxsmtplay/ex.smt"
    val unsat = solver.solveFromFile(file)
    */
     val solver = new FileDriver with Linear with Circuit with Z3
    val file = "/home/shinhwei/workspace/z3/maxsmt-playground/src/main/scala/edu/nus/maxsmtplay/ex.smt"
    val unsat = solver.solveFromFile(file)
    //val unsat = solver.solveTest()
    println(unsat)
    //smtlib_maxsat(file,0)
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
