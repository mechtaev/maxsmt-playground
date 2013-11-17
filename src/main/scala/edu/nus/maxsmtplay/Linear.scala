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
    println("checking whether hard constraints are satisfiable...")
    if (!sat) {
      println("result: " + -1)
      return List()
    }

    if (soft.size == 0) {
      println("result: " + 0)
      return List()

    }
    var aux = assertAssumptions(soft).map(_._2)

    var k = soft.size - 1
    while (true) {
      println("checking whether at-most " + k + " soft-constraints can be ignored.")
      atMostK(aux, k)
      val Some(result) = solver.check()
      if (!result) {
        println("unsat")
        println("result: " + (soft.size - k - 1))
        return List()
      }
      val numDisabled = getNumDisabledSoftConstraint(aux)
      println("number of disabled:" + numDisabled)
      if (numDisabled > k)
        throw new Exception("Number of disabled constraints is more than k")
      println("sat")
      k = numDisabled
      if (k == 0) {
        println("result: " + soft.size)
        return List()
      }
      k = k - 1
    }

    println("result: " + 0)
    List()
  }
  /**
    * Return the number of soft-constraints that were disabled by the given model.
    * A soft-constraint was disabled if the associated auxiliary variable was assigned to true.
    */
  def getNumDisabledSoftConstraint(aux: List[Z3AST]): Int = {
    val model = solver.getModel()
    val t = z3.mkTrue
    var disabled = 0
    var i = 0
    while (i < aux.length) {
      val Some(result) = model.eval(aux(i))
      print(aux(i))
      print("->")
      printlnAST(result)
      if (result.equals(t)) {
        disabled = disabled + 1
      }
      i = i + 1
    }
    disabled
  }

}
