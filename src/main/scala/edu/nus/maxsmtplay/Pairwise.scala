package edu.nus.maxsmtplay

import com.microsoft.z3._

trait Pairwise extends AtMostOne {
  this: Z3 =>

  override def atMostOne(cs: List[BoolExpr]) = {
    def allPairs(l: List[BoolExpr]): List[(BoolExpr, BoolExpr)] = {
      l match {
        case h :: t => t.map({ e => (h, e) }) ++ allPairs(t)
        case List() => List()
      }
    }
    val pairs = allPairs(cs)
    val disjunctions = pairs.map({ 
      case (a, b) => z3.mkOr(z3.mkNot(a), z3.mkNot(b)) 
    })
    disjunctions.map({ c => solver.add(c) })
  }
}
