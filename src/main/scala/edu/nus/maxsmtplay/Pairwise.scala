package edu.nus.maxsmtplay

import z3.scala._

trait Pairwise extends AtMostOne {
  this: Z3 =>

  override def atMostOne(cs: List[Z3AST]) = {
    def allPairs(l: List[Z3AST]): List[(Z3AST, Z3AST)] = {
      l match {
        case h :: t => t.map({ e => (h, e) }) ++ allPairs(t)
        case List() => List()
      }
    }
    val pairs = allPairs(cs)
    val disjunctions = pairs.map({ case (a, b) => z3.mkOr(z3.mkNot(a), z3.mkNot(b))})
    val cnf = disjunctions.reduceLeft(z3.mkAnd(_, _))
    solver.context.assertCnstr(cnf)
  }
}