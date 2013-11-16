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
    var aux = soft.map(c => (z3.mkFreshConst("k", z3.mkBoolSort)))
    var softAndAux = soft.zip(aux)
    //assert soft and aux:
    var auxPair = softAndAux.map({ case (c, a) => z3.mkOr(c, a) })
    auxPair.map(solver.assertCnstr)
    
    var k = soft.size - 1
    while (true) {
     // println("Before....................")
      //aux.map({ ast => println(z3.astToString(ast))})
      //println("After.....................")
      println("checking whether at-most "+k+" soft-constraints can be ignored.")
      atMostK(aux, k)
      val resultAndModel = z3.checkAndGetModel()
      val Some(result) = resultAndModel._1
      val model = resultAndModel._2
      if (!result) {
        println("unsat")
        println("result: " + (soft.size - k - 1))
        return List()
      }
      //aux.map({ ast => println("aux:"+z3.astToString(ast)) })
      val numDisabled = getNumDisabledSoftConstraint(model, aux)
      println("number of disabled:" + numDisabled)
      if (numDisabled > k)
        throw new Exception("Number of disabled constraints is more than k")
     model.delete
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
  def getNumDisabledSoftConstraint(model: Z3Model, aux: List[Z3AST]): Int = {

    val t = z3.mkTrue
    var disabled = 0
    var i = 0
    //Some(result)
    while (i < aux.length) {
      val Some(result) = model.eval(aux(i))
      print(aux(i))
      print("->")
      printlnAST(result)
      //if (result) {
        if (result.equals(t)) {
          disabled = disabled + 1
        }
      //}
      i = i + 1
    }
  /*
    aux.count((x: Z3AST) => {
      val Some(result) = model.evalAs[Boolean](x)
      result && Some(result).eq(t)
    })*/
    disabled
  }

}
