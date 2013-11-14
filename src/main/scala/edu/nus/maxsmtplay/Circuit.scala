package edu.nus.maxsmtplay

import z3.scala._
import scalaz._

/**
 * Interface of AtMostK constraint
 */
trait Circuit extends AtMostK {
  this: Z3 =>

  override def atMostK(cs: List[Z3AST], k:Int): Unit = {
    //TODO
    println("inside cir")
    val n = cs.length
    if (k >= n || n <= 1)
      return
    val counterBits = mkCounterCircuit(cs)
    assertLEK(counterBits,k)
  }
  
  def assertLEK(value: List[Z3AST], k:Int): Unit ={
    var not_val =  z3.mkNot(value(0))
    var out = not_val
    if(getBit(k,0))
      out =  z3.mkTrue
      //TODO finish this
    var index = 1
    var i1 = z3.mkFalse()
    var i2 = z3.mkFalse()
    while(index< value.length ){
        not_val = z3.mkNot(value(index))
        if (getBit(k, index)) {
            i1 = not_val
            i2 = out
        }
        else {
            i1 = z3.mkFalse()
            i2 = z3.mkFalse()
        }
        out = mkTernaryOr(i1, i2, z3.mkAnd(not_val, out));
        index = index+1
    }

     z3.assertCnstr(out)
  }
  
  def getBit(value:Int, idx:Int): Boolean ={
    val mask = 1 << (idx & 31);
    (value & mask) != 0;
 }

  
  def mkCounterCircuit(cs: List[Z3AST]): List[Z3AST] = {
    var index = cs.length;
    var tempList = List[Z3AST]()
     while(index>1){
    	tempList = tempList ++ mkAdderPairs(cs)
    	index=half(index)
    }
    tempList
  }

  def mkAdderPairs(in: List[Z3AST]): List[Z3AST] ={
    //def pairList(l: List[Z3AST]):List[Z3AST] = {
      in match{
        case a::b::tl => mkAdder(List[Z3AST](a),List[Z3AST](b)) ++ mkAdderPairs(tl)
        case a::List() => mkAdder(List[Z3AST](a),List[Z3AST](z3.mkFalse()))
        case List() => List()
      }
    //}
  }

  def mkAdder(in1: List[Z3AST],in2: List[Z3AST]) : List[Z3AST] ={
    val cin = z3.mkFalse()
    val out = in1.zip(in2).map({case (a,b)=>mkFullAdder(a,b,cin)})
    val (result, _) = 
      in1.zip(in2).foldLeft((List[Z3AST](), cin))(
    		{case ((prevResult, prevAdder), (a, b)) => 
    		  	  	val (nextResult, nextAdder) = mkFullAdder(a, b, prevAdder)
    		  	  	(prevResult ++ List(nextResult), nextAdder)
    		})
    result  
   }
  
   def mkFullAdder(in1: Z3AST,in2: Z3AST, cin: Z3AST) : (Z3AST,Z3AST) ={
    val fst = mkTernaryOr(z3.mkAnd(in1,in2),z3.mkAnd(in1,cin),z3.mkAnd(in2,cin))
    val snd = z3.mkXor(z3.mkXor(in1,in2), cin)
    (fst,snd)
  }
  
   def mkTernaryOr(in1: Z3AST,in2: Z3AST, in3: Z3AST): Z3AST = {
     z3.mkOr(in1,z3.mkOr(in2,in3))
   }
  
  private def half(i: Int): Int = {
   var n = i / 2
    if(i%2!=0)
     n = (i/ 2) +1 
    n
  }
  
}






