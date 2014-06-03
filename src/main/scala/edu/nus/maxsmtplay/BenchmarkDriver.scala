package edu.nus.maxsmtplay

import com.microsoft.z3._

class BenchmarkDriver(name: String, solver: SolverTestUtils with Z3 with Printer with Verifier) {

  def init(timeout: Option[Int]) = {
    solver.init(timeout)
  }

  def delete() = {
    solver.delete()
  }

  def measureTime[A](f: => A) = {
    val s = System.nanoTime
    val result = f
    // result and time in ms
    (result, ((System.nanoTime-s)/1e6).toInt)
  }

  def solveBenchmark(filename: String): (List[BoolExpr], Boolean, Int, Int, Int) = {
    val ((maxsat, soft, hard), time) = measureTime(solver.solveFromFileAndGetSoftHard(filename))
    val consistent = solver.checkSat(maxsat)
    (maxsat, consistent, time, hard.size, hard.size + soft.size)
  }

  def runAndPrint(filename: String, answer: Int) = {
    println("-----------------------------------------")
    println("Solver " + name)
    println("Benchmark " + filename)
    solveBenchmark(filename) match {
      case (maxsat, consistent, time, hard, size) => 
        if (!consistent) {
          println("ERROR")
          println("Size " + hard + "->" + maxsat.size + "->" + size)
          solver.writeLog("benchmark-error", maxsat.toString)
        } else {
          if (maxsat.size == answer) println("MAX " + hard + "->" + maxsat.size + "->" + size)
          else println("MCS " + hard + "->" + maxsat.size + "->" + size)
          println("Time " + time + " ms")
        }
    }
  }
}
