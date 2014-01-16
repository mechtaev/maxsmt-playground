package edu.nus.maxsmtplay

import com.microsoft.z3._
import scalaz._

/**
 * Interface of AtMostK constraint
 */
trait Circuit extends AtMostK {
  this: Z3 =>

  override def atMostK(constraints: List[BoolExpr], k: Int): Unit = {
    val n = constraints.length
    if (k >= n || n <= 1)
      return
    val counterBits = mkCounterCircuit(constraints)
    assertLEK(counterBits, k)
  }

  def assertLEK(value: List[BoolExpr], k: Int): Unit = {
    var notVal = z3.mkNot(value(0))
    var out = notVal
    if (getBit(k, 0))
      out = z3.mkTrue
    var index = 1
    while (index < value.length) {
      var i1 = z3.mkFalse()
      var i2 = z3.mkFalse()
      notVal = z3.mkNot(value(index))
      if (getBit(k, index)) {
        i1 = notVal
        i2 = out
      } else {
        i1 = z3.mkFalse()
        i2 = z3.mkFalse()
      }
      out = mkTernaryOr(i1, i2, z3.mkAnd(notVal, out))
      index = index + 1
    }
    //println("at-most-k:")
    //printlnAST(out)
    solver.add(out)
  }

  def getBit(value: Int, idx: Int): Boolean = {
    val mask = 1 << (idx & 31)
    (value & mask) != 0
  }

  def mkCounterCircuit(cs: List[BoolExpr]): List[BoolExpr] = {
    var numIns = cs.length
    var tempList = cs
    var numBits = 1
    while (numIns > 1) {
      val (retList, newNumIns) = mkAdderPairs(tempList, numIns, numBits)
//      println("newNumIns: " + newNumIns)
      numIns = newNumIns
      numBits = numBits + 1
      tempList = retList
    }
    tempList
  }

  def mkAdderPairs(in: List[BoolExpr], numIns: Int, numBits: Int): (List[BoolExpr], Int) = {
    var output = List[BoolExpr]()
    var outCopy = List[BoolExpr]()
    var inCopy = List[BoolExpr]()
    var input = in
    var index = 0
    val outNumBits = numBits + 1
    val outNumIns = half(numIns)
    
    while (index < numIns / 2) {
//      println("numBits:" + numBits)
      val result = mkAdder(input, input.drop(numBits), numBits)
      output = result
      inCopy = inCopy ++ in
      outCopy = outCopy ++ result
      input = input.drop(numBits * 2)
      output = output.drop(outNumBits)
      index = index + 1
    }
    if (numIns % 2 != 0) {
      var i = 0
      while (i < numBits) {
        outCopy = outCopy ++ List(input(i))
        i = i + 1
      }
      outCopy = outCopy ++ List(z3.mkFalse)
      return (outCopy, outNumIns)
    }
    return (outCopy, outNumIns)
  }

  /**
    * Create an adder of two bit arrays
    */
  def mkAdder(in1: List[BoolExpr], in2: List[BoolExpr], numBits: Int): List[BoolExpr] = {
    var cin = z3.mkFalse()
    var index = 0
    var result = List[BoolExpr]()
    while (index < numBits) {
      val (carryout, out) = mkFullAdder(in1(index), in2(index), cin)
      result = result ++ List(out)
      cin = carryout
      index = index + 1
    }
    result = result ++ List(cin)
    result
  }

  /** 
    * Constructs a full adder for two bits
    * return a pair of (cout, out)
    */
  def mkFullAdder(in1: BoolExpr, in2: BoolExpr, cin: BoolExpr): (BoolExpr, BoolExpr) = {
    val cout = 
      mkTernaryOr(z3.mkAnd(in1, in2), z3.mkAnd(in1, cin), z3.mkAnd(in2, cin))
    val out = z3.mkXor(z3.mkXor(in1, in2), cin)
    (cout, out)
  }

  def mkTernaryOr(in1: BoolExpr, in2: BoolExpr, in3: BoolExpr): BoolExpr = {
    z3.mkOr(in1, in2, in3)
  }

  private def half(i: Int): Int = {
    var n = i / 2
    if (i % 2 != 0)
      n = (i / 2) + 1
    n
  }

}






