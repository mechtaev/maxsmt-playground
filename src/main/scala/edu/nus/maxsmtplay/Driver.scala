package edu.nus.maxsmtplay

import z3.scala._
import java.io.File

/**
 * Entry point
 */
object Driver {

  def main(args: Array[String]): Unit = {
    // Initialize solver
    val solver = new FileDriver 
      with Linear with Circuit with Z3
      //with FuMalik with Pairwise with Z3
      //with FastDiag with Z3
    solver.init()

    val file = new File(".").getAbsolutePath() + "/benchmarks/ex.smt"
    val maxsat = solver.solveFromFile(file)
    println("Result:")
    solver.printConstraints(maxsat)

    // delete solver:
    solver.delete()
  }

}
