
package edu.nus.maxsmtplay

import z3.scala._

trait MaxSMT {

  def solve(soft: List[Z3AST], hard: List[Z3AST]): List[Z3AST]

}
