package edu.nus.maxsmtplay

import z3.scala._
import z3.scala.dsl._
/**
 * Implementation of linear algorithm
 */
trait Linear extends MaxSMT {
  this: Z3 with AtMostK =>

  override def solve(soft: List[Z3AST], hard: List[Z3AST]): Int = {
    println("in linear")
    hard.map((c: Z3AST) => solver.assertCnstr(c))
    val Some(sat) = solver.check()
    if (!sat) {
      return -1
    }

    if (soft.size == 0)
      return 0 //nothing to be done
    var softAndAux = soft.map(c => (c, z3.mkFreshConst("a", z3.mkBoolSort)))
    //assert soft and aux:
    var aux = softAndAux.map({ case (c, a) => z3.mkOr(c, a) })
    aux.map(solver.assertCnstr)
    var k = soft.size - 1
    while (true) {
      println("Before....................")
      aux.map({ ast => println(z3.astToString(ast))})
       println("After.....................")
      atMostK(aux, k)
      println("k:"+k)
      
      val resultAndModel = z3.checkAndGetModel()
      val result = resultAndModel._1
      val model = resultAndModel._2
      result match {
          case None => println("There was an error with Z3.");
          case Some(false) => println(soft.size - k - 1) ; (return soft.size - k -1 ) // formula was unsat
          case Some(true) => println("sat");
        }

      val numDisabled = getNumDisabledSoftConstraint(model, aux)
      println("number of disabled:"+numDisabled)
      if (numDisabled > k)
        throw new Exception("Number of disabled constraints is more than k")
      k = numDisabled
      if (k == 0)
        return soft.size
      k = k - 1
    }

    0
  }
  /**
   * Return the number of soft-constraints that were disable by the given model.
   * A soft-constraint was disabled if the associated auxiliary variable was assigned to true.
   */
  def getNumDisabledSoftConstraint(model: Z3Model, aux: List[Z3AST]): Int = {
   
     val t = z3.mkTrue
     var disabled = 0
     var i = 0
    //Some(result)
     while(i<aux.length){
       val Some(result) = model.evalAs[Boolean](aux(i))
       if(result){
         if(Some(result).equals(t)){
           disabled= disabled + 1
         }
       }
     }
     /*
    aux.count((x: Z3AST) => {
      val Some(result) = model.evalAs[Boolean](x)
      result && Some(result).eq(t)
    })*/
     disabled
  }

}
