package edu.nus.maxsmtplay

import z3.scala._

class BenchmarkDriver(name: String) {
  this: Z3 with MaxSMT =>

  def measureTime[A](f: => A) = {
    val s = System.nanoTime
    val result = f
    // result and time in ms
    (result, ((System.nanoTime-s)/1e6).toInt)
  }

  def check(constraints: List[Z3AST]): Boolean = {
    solver.reset()
    solver.assertCnstr(z3.mkAnd(constraints:_*))
    val Some(result) = solver.check()
    result
  }

  def solveBenchmark(filename: String): (Int, Int, Boolean, Int) = {
    z3.parseSMTLIBFile(filename)
    val hard = z3.getSMTLIBFormulas.toList
    val soft = z3.getSMTLIBAssumptions.toList
    val (maxsat, time) = measureTime(solve(soft, hard))
    val consistent = check(maxsat)
    (maxsat.size, hard.size + soft.size, consistent, time)
  }

  def solveAndPrint(filename: String, answer: Int) = {
    println("-----------------------------------------")
    println("Solver " + name)
    println("Benchmark " + filename)
    solveBenchmark(filename) match {
      case (maxsatSize, size, consistent, time) => 
        if (!consistent) {
          println("ERROR")
        } else {
          if (size == answer) println("MAX " + maxsatSize + "/" + size)
          else println("MCS " + maxsatSize + "/" + size)
          println("Time " + time + " ms")
        }
    }
  }

}
