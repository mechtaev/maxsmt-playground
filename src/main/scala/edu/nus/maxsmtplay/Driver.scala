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
    val solver =
      //new Linear(Some(10)) with Circuit with Z3 with Printer with Verifier
      new FuMalik with Circuit with Z3 with Printer with Verifier
      //new FuMalik with Pairwise with Z3 with Printer with Verifier
      //new FastDiag with Z3 with Printer with Verifier
    solver.init()

    val filePath = new File("benchmarks/repair-linear.smt").getAbsolutePath()
    val maxsat = solver.solveFromFile(filePath)

    //solver.printConstraints("" + maxsat.size + " constraints:", maxsat)

    solver.checkSat(maxsat) match {
      case false => println("ERROR")
      case true => println("OK")
    }

    // Delete:
    solver.delete()

  }

  def executeBenchmarks() = {
    def path(f: String): String = {
      new File("benchmarks/" + f).getAbsolutePath()
    }
    val benchmarks =
      ("ex.smt",             5) ::
      ("repair-cubes.smt",   1358) ::
      ("repair-square.smt",  631) ::
      ("pigeon-hole-10.smt", 240) ::
      ("repair-linear.smt",  2885) :: 
        List()
    val drivers =
      new BenchmarkDriver(
        "Fu-Malik Pairwise",
        new FuMalik with Pairwise with Z3 with Printer with Verifier) ::
      new BenchmarkDriver(
        "Fu-Malik Circuit",
        new FuMalik with Circuit with Z3 with Printer with Verifier) ::
      // new BenchmarkDriver(
      //   "Linear",
      //   new Linear(None) with Circuit with Z3 with Printer with Verifier) ::
      new BenchmarkDriver(
        "Linear 5", 
        new Linear(Some(5)) with Circuit with Z3 with Printer with Verifier) ::
      new BenchmarkDriver(
        "Fast-Diag", 
        new FastDiag with Z3 with Printer with Verifier) ::
        List()
    benchmarks.map({case (f, a) => drivers.map(d => {
      d.init()
      d.runAndPrint(path(f), a)
      d.delete()
    })})
  }

}
