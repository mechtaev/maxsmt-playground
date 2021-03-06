package edu.nus.maxsmtplay

import com.microsoft.z3._
import java.io.File

/**
 * Entry point
 */
object Driver {

  def main(args: Array[String]): Unit = {
    // test()
    executeBenchmarks()
  }

  def test() {
    // Initialize solver
    val solver =
      // new Linear(Some(10)) with Circuit with Z3 with Printer with Verifier
      new FuMalik(Some(5)) with Circuit with Z3 with Printer with Verifier with SolverTestUtils
      // new FuMalik with Pairwise with Z3 with Printer with Verifier
      // new FastDiag with Z3 with Printer with Verifier
    println("init...")
    solver.init(None)

    val filePath = new File("benchmarks/ex.smt").getAbsolutePath()
    println(filePath)
    val maxsat = solver.solveFromFile(filePath)

    solver.printConstraints("" + maxsat.size + " constraints:", maxsat)

    println("verifying...")
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
      ("ex.smt"              , 5) ::
      ("repair-cubes-2.smt"  , 1358) ::
      ("repair-square-2.smt" , 631) ::
      ("pigeon-hole-5.smt"   , 70) ::
      ("pigeon-hole-10.smt"  , 240) ::
      ("pigeon-hole-20.smt"  , 880) ::
      ("repair-linear-2.smt" , 2885) ::
        List()
    val drivers =
      new BenchmarkDriver(
       "Fu-Malik Pairwise",
       new FuMalik(Some(5)) with Pairwise with Z3 with Printer with Verifier with SolverTestUtils) ::
      new BenchmarkDriver(
       "Fu-Malik Circuit",
       new FuMalik(Some(5)) with Circuit with Z3 with Printer with Verifier with SolverTestUtils) ::
      // new BenchmarkDriver(
      //   "Linear",
      //   new Linear(None) with Circuit with Z3 with Printer with Verifier with SolverTest) ::
      new BenchmarkDriver(
        "Linear 5", 
        new Linear(Some(5)) with Circuit with Z3 with Printer with Verifier with SolverTestUtils) ::
      new BenchmarkDriver(
         "Fast-Diag", 
         new FastDiag with Z3 with Printer with Verifier with SolverTestUtils) ::
        List()
    benchmarks.map({case (f, a) => drivers.map(d => {
      d.init(None)
      d.runAndPrint(path(f), a)
      d.delete()
    })})
  }

}
