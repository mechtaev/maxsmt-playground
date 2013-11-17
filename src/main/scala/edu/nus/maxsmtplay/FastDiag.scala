package edu.nus.maxsmtplay

import z3.scala._
import scala.collection.mutable.Stack

/**
  * Implementation of Fast-Diag algorithm
  *
  * For more information on the Fast-Diag procedure:
  *
  * Felfernig, A. and Schubert, M. and Zehentner, C. An Efficient Diagnosis
  * Algorithm for Inconsistent Constraint Sets, Artif. Intell. Eng. Des. Anal.
  * Manuf. P. 53-62. 2012.
  */
trait FastDiag extends MaxSMT {
  this: Z3 =>

  override def solve(soft: List[Z3AST], hard: List[Z3AST]): List[Z3AST] = {
    val softAssumptions = assertAssumptions(soft)
    val softAux = softAssumptions.map(_._2)
    val hardAssumptions = assertAssumptions(hard)
    val hardAux = hardAssumptions.map(_._2)
    val mcs = fastDiagOpt(softAux ++ hardAux, softAux, false)
    softAssumptions.filter({case (s, a) => !mcs.contains(a)}).map(_._1) ++ hard
  }

  private def fastDiagNaive(r: List[Z3AST], t: List[Z3AST], hasD: Boolean): List[Z3AST] = {
    val Some(sat) = solver.checkAssumptions(r.map(z3.mkNot):_*)
    if (hasD && sat) 
      return List()
    if (t.size == 1)
      return t
    val m = t.size / 2
    val (tLeft, tRight) = (t.slice(0, m), t.slice(m, t.size))
    val rMinusTLeft = r.filter((ast: Z3AST) => !tLeft.contains(ast))
    val dRight = fastDiagNaive(rMinusTLeft, tRight, tLeft.size != 0)
    val rMinusDRight = r.filter((ast: Z3AST) => !dRight.contains(ast))
    val dLeft = fastDiagNaive(rMinusDRight, tLeft, dRight.size != 0)
    dLeft ++ dRight
  }

  sealed trait Part
  case class First(rp: List[Z3AST], rm: List[Z3AST], t: List[Z3AST], hadD: Boolean) extends Part
  case class Second(rp: List[Z3AST], t: List[Z3AST]) extends Part
  case class Sum() extends Part

  val args = new Stack[Part]
  val results = new Stack[List[Z3AST]]

  private def fastDiagOpt(r: List[Z3AST], t: List[Z3AST], hasD: Boolean): List[Z3AST] = {
    args.push(First(r, List(), t, hasD))
    var count = 0
    while(!args.isEmpty && count < 200) {
      count += 1

      val arg = args.pop()
      arg match {
        case First(rp, rm, t, hasD) => {
          val r = rp.filter((ast: Z3AST) => !rm.contains(ast))
          print("1")
          val Some(sat) = solver.checkAssumptions(r.map(z3.mkNot):_*)
          if (hasD && sat) results.push(List())
          else if (t.size == 1) results.push(t)
          else {
            val m = t.size / 2
            val (tLeft, tRight) = (t.slice(0, m), t.slice(m, t.size))
            args.push(Sum())
            args.push(Second(r, tLeft))
            args.push(First(r, tLeft, tRight, tLeft.size != 0))
          }     
        }
        case Second(rp, t) => {
          val dRight = results.top
          val hasD = (dRight.size != 0)
          val rm = dRight
          val r = rp.filter((ast: Z3AST) => !rm.contains(ast))
          print("2")
          val Some(sat) = solver.checkAssumptions(r.map(z3.mkNot):_*)
          if (hasD && sat) results.push(List())
          else if (t.size == 1) results.push(t)
          else {
            val m = t.size / 2
            val (tLeft, tRight) = (t.slice(0, m), t.slice(m, t.size))
            args.push(Sum())
            args.push(First(r, tLeft, tRight, tLeft.size != 0))
            args.push(Second(r, tLeft))
          }
        }
        case Sum() => {
          val l = results.pop()
          val r = results.pop()
          results.push(l ++ r)
        }
      }
    }
    results.top
  }

}
