package edu.nus.maxsmtplay

import com.microsoft.z3._

/**
  * Implementation of linear algorithm
  */
abstract class Linear(start: Option[Int]) extends MaxSMT {
  this: Z3 with AtMostK =>

  //TODO case when we do not have any solution
  override def solveAndGetModel(soft: List[BoolExpr], hard: List[BoolExpr]): Option[(List[BoolExpr], Model)] = {
    hard.map((c: BoolExpr) => solver.add(c))

    //FIXME should I check satisfiability here?
    // val Some(sat) = solver.check()
    // if (!sat) {
    //   throw new Exception("Hard constraints are not satisfiable")
    // }

    //FIXME does this check make sense?
    // if (soft.size == 0) {
    //   return hard
    // }
    var assumptions = assertAssumptions(soft)
    var aux = assumptions.map(_._2)
    var result = List[BoolExpr]()
    var model: Model = null

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
        //FIXME is model still available here?
        return Some((hard ++ result, model))
      }
      val numDisabled = getNumDisabledSoftConstraint(aux)
      if (numDisabled > k)
        throw new Exception("Number of disabled constraints is more than k")
      k = numDisabled
      if (k == 0) {
        throw new Exception("This place we should not reach")
        //return hard ++ soft
      }
      k = k - 1

      model = solver.getModel()
      result = assumptions.filter({
        case (s, a) => {
          val value = model.eval(a, true)
          value.equals(z3.mkFalse)
        }
      }).map(_._1)
    }

    ???
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
