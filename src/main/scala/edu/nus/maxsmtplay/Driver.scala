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

}
