package edu.nus.maxsmtplay

import z3.scala._
import z3.scala.dsl._
/**
  * Implementation of linear algorithm
  */
trait Linear extends MaxSMT {
  this: Z3 with AtMostK =>

  override def solve(soft: List[Z3AST], hard: List[Z3AST]): List[Z3AST] = {
    hard.map((c: Z3AST) => solver.assertCnstr(c))
    val Some(sat) = solver.check()
    if (!sat) {
      throw new Exception("Hard constraints are not satisfiable")
    }
    if (soft.size == 0) {
      return hard
    }
    var assumptions = assertAssumptions(soft)
    var aux = assumptions.map(_._2)
    var result = List[Z3AST]()

    var k = soft.size - 1
    while (true) {
      atMostK(aux, k)
      val Some(sat) = solver.check()
      if (!sat) {
        //removing (soft.size - k - 1) constraints
        return hard ++ result
      }
      val numDisabled = getNumDisabledSoftConstraint(aux)
      if (numDisabled > k)
        throw new Exception("Number of disabled constraints is more than k")
      k = numDisabled
      if (k == 0) {
        return hard ++ soft
      }
      k = k - 1

      val model = solver.getModel()
      result = assumptions.filter({
        case (s, a) => {
          val Some(value) = model.eval(a)
          value.equals(z3.mkFalse)
        }
      }).map(_._1)
    }

    assert(false)
    List()
  }

  /**
    * Return the number of soft-constraints that were disabled by the given model.
    * A soft-constraint was disabled if the associated auxiliary variable was assigned to true.
    */
  def getNumDisabledSoftConstraint(aux: List[Z3AST]): Int = {
    val model = solver.getModel()
    aux.filter(a => {
      val Some(result) = model.eval(a)
      result.equals(z3.mkTrue)
    }).size
  }

}
