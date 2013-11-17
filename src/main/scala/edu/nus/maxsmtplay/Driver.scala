package edu.nus.maxsmtplay

import z3.scala._
import java.io.File

/**
 * Entry point
 */
object Driver {

  def main(args: Array[String]): Unit = {
    //test()
    executeBenchmarks()
  }

  def test() {
    // Initialize solver
    val solver = new FileDriver 
      //with Linear with Circuit with Z3
      //with FuMalik with Pairwise with Z3
      with FastDiag with Z3
    solver.init()

    val file = new File(".").getAbsolutePath() + "/benchmarks/ex.smt"
    val maxsat = solver.solveFromFile(file)
    solver.printConstraints(maxsat)

    // Delete solver:
    solver.delete()

  }

  def executeBenchmarks() = {
    def path(f: String): String = {
      new File(".").getAbsolutePath() + "/benchmarks/" + f
    }
    val benchmarks =
      ("ex.smt",             5) ::
      ("repair-cubes.smt",   0) ::
      ("repair-square.smt",  0) ::
      ("pigeon-hole-10.smt", 0) ::
      ("repair-linear.smt",  0) :: 
        List()
    val solvers =
      new BenchmarkDriver("Fu-Malik Pairwise") with FuMalik with Pairwise with Z3 ::
      new BenchmarkDriver("Fu-Malik Circuit") with FuMalik with Circuit with Z3 ::
      //new BenchmarkDriver("Linear") with Linear with Circuit with Z3 ::
      new BenchmarkDriver("Fast-Diag") with FastDiag with Z3 ::
        List()
    benchmarks.map({case (f, a) => solvers.map(s => {
      s.init()
      s.solveAndPrint(path(f), a)
      s.delete()
    })})
  }

}
