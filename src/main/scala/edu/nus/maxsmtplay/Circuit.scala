package edu.nus.maxsmtplay

import z3.scala._
import scalaz._

/**
 * Interface of AtMostK constraint
 */
trait Circuit extends AtMostK {
  this: Z3 =>

  override def atMostK(cs: List[Z3AST], k: Int): Unit = {
    val n = cs.length
    if (k >= n || n <= 1)
      return
    val counterBits = mkCounterCircuit(cs)
    assertLEK(counterBits, k)
  }

  def assertLEK(value: List[Z3AST], k: Int): Unit = {
    var not_val = z3.mkNot(value(0))
    var out = not_val
    if (getBit(k, 0))
      out = z3.mkTrue
    var index = 1
   // value.map({ ast => println("val:" + z3.astToString(ast)) })
    while (index < value.length) {
      var i1 = z3.mkFalse()
      var i2 = z3.mkFalse()
      // if(value(index).getSort == z3.mkBoolSort)
      not_val = z3.mkNot(value(index))
      if (getBit(k, index)) {
        i1 = not_val
        i2 = out
      } else {
        i1 = z3.mkFalse()
        i2 = z3.mkFalse()
      }
      out = mkTernaryOr(i1, i2, z3.mkAnd(not_val, out))
      index = index + 1
    }
   // println("out final:" + z3.astToString(out))
    solver.context.assertCnstr(out)
  }

  def getBit(value: Int, idx: Int): Boolean = {
    val mask = 1 << (idx & 31)
    (value & mask) != 0
  }

  def mkCounterCircuit(cs: List[Z3AST]): List[Z3AST] = {
    var num_ins = cs.length
    var tempList = cs
    var aux_2 = List[Z3AST]()

    var num_bits = 1
    while (num_ins > 1) {
      val (retList, new_num_ins) = mkAdderPairs(tempList, num_ins, num_bits)
      aux_2 = retList
      //tempList = tempList ++ retList
      //println("tempList size:"+tempList.size)
      num_ins = new_num_ins
      num_bits = num_bits + 1
     // println("num_bits:" + num_bits + " num_ins:" + num_ins)
      //aux_2.map({ ast => println("bits:" + z3.astToString(ast)) })

      val tmp = tempList
      tempList = aux_2
      //tempList.map({ ast => println("after bits:" + z3.astToString(ast)) })
      aux_2 = tmp
    }
    tempList
  }

  def mkAdderPairs(in: List[Z3AST], num_ins: Int, num_bits: Int): (List[Z3AST], Int) = {
    var _out = List[Z3AST]()
    var outCopy = List[Z3AST]()
    var inCopy = List[Z3AST]()
    var _in = in
    var index = 0
    val out_num_bits = num_bits + 1
    val out_num_ins = half(num_ins)
    /*
    def pairList(l: List[Z3AST]):List[Z3AST] = {
      in match{
        case a::b::tl => mkAdder(List[Z3AST](a),List[Z3AST](b),num_bits) ++ mkAdderPairs(tl,num_ins,num_bits+1)._1
        case a::List() => mkAdder(List[Z3AST](a),List[Z3AST](z3.mkFalse()),num_bits)
        case List() => List()
      }
    }
    pairList(in)
     */

    while (index < num_ins / 2) {
      val result = mkAdder(_in, _in.drop(num_bits), num_bits)
      _out = result
      inCopy = inCopy ++ in
      outCopy = outCopy ++ result
      //result.map({ ast => println("here:"+z3.astToString(ast))})
      //_out.map({ ast => println("out here:"+z3.astToString(ast))})
       _in = _in.drop(num_bits)
       _in = _in.drop(num_bits)
       _out = _out.drop(out_num_bits)
       // println("out after size:"+_out.size)
     // _out.map({ ast => println("out after here:"+z3.astToString(ast))})
      index = index + 1
    }
    //outCopy = List[Z3AST]()
    if (num_ins % 2 != 0) {
      var i = 0
      while (i < num_bits) {
        outCopy = outCopy ++ List(_in(i))
        i = i + 1
      }
      outCopy = outCopy ++ List(z3.mkFalse)
      //println("outCopy:"+outCopy.size)
      //_in.map({ ast => println("inCopy here:" + z3.astToString(ast)) })
      //outCopy.map({ ast => println("out here:" + z3.astToString(ast)) })
      return (outCopy, out_num_ins)
    }
    //println("outsize:"+outCopy.size)
   // _out.map({ ast => println("here:" + z3.astToString(ast)) })
    return (outCopy, out_num_ins)
  }

  def mkAdder(in1: List[Z3AST], in2: List[Z3AST], num_bits: Int): List[Z3AST] = {
    //in1.map({ ast => println("in1:"+z3.astToString(ast))})
    //in2.map({ ast => println("in2:"+z3.astToString(ast))})
    var cin = z3.mkFalse()
    var cout = z3.mkFalse()
    var index = 0
    var result = List[Z3AST]()
    while (index < num_bits) {
     // println("i: " + index + "num_bits: " + num_bits)
     // println("in1 " + index + ":" + z3.astToString(in1(index)))
      //println("in2 " + index + ":" + z3.astToString(in2(index)))
      val (carryout, out) = mkFullAdder(in1(index), in2(index), cin)
      cout = carryout
      result = result ++ List(out)
      cin = cout
      index = index + 1
    }
    result = result ++ List(cout)
    /*
    val out = in1.zip(in2).map({case (a,b)=>mkFullAdder(a,b,cin)})
    val (result, _) = 
      in1.zip(in2).foldLeft((List[Z3AST](), cin))(
    		{case ((prevResult, prevAdder), (a, b)) => 
    		  	  	val (nextResult, nextAdder) = mkFullAdder(a, b, prevAdder)
    		  	  	(prevResult ++ List(nextResult), nextAdder)
    		})
    		*/
    result
  }

  def mkFullAdder(in1: Z3AST, in2: Z3AST, cin: Z3AST): (Z3AST, Z3AST) = {
    val cout = mkTernaryOr(z3.mkAnd(in1, in2), z3.mkAnd(in1, cin), z3.mkAnd(in2, cin))
    val out = z3.mkXor(z3.mkXor(in1, in2), cin)
    (cout, out)
  }

  def mkTernaryOr(in1: Z3AST, in2: Z3AST, in3: Z3AST): Z3AST = {
    z3.mkOr(in1, in2, in3)
  }

  private def half(i: Int): Int = {
    var n = i / 2
    if (i % 2 != 0)
      n = (i / 2) + 1
    n
  }

}






