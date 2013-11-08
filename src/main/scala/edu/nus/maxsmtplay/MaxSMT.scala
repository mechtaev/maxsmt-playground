package edu.nus.maxsmtplay

import z3.scala._

trait MaxSMT {

  def solve(soft: Seq[Z3AST], hard: Seq[Z3AST]): Int

}
