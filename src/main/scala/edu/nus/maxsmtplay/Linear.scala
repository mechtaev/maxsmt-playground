package edu.nus.maxsmtplay

import com.microsoft.z3._

/**
  * Implementation of linear algorithm
  */
abstract class Linear(start: Option[Int]) extends MaxSMT {
  this: Z3 with AtMostK =>

  override def solve(soft: List[BoolExpr], hard: List[BoolExpr]): List[BoolExpr] = {
    hard.map((c: BoolExpr) => solver.add(c))
    // val Some(sat) = solver.check()
    // if (!sat) {
    //   throw new Exception("Hard constraints are not satisfiable")
    // }
    if (soft.size == 0) {
      return hard
    }
    var assumptions = assertAssumptions(soft)
    var aux = assumptions.map(_._2)
    var result = List[BoolExpr]()

    var k = start match {
      case None => soft.size - 1
      case Some(v) => v
    }
    while (true) {
      atMostK(aux, k)
      val checkResult = solver.check()
      val sat = (checkResult == Status.SATISFIABLE)
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
          val value = model.eval(a, true)
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
  def getNumDisabledSoftConstraint(aux: List[BoolExpr]): Int = {
    val model = solver.getModel()
    aux.filter(a => {
      val result = model.eval(a, true)
      result.equals(z3.mkTrue)
    }).size
  }

}
