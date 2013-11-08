package edu.nus.maxsmtplay

import z3.scala._
import org.scalatest._

class TestZ3 extends FlatSpec with Matchers {

  "Z3 solver" should "get UNSAT answer for the Z3 MaxSAT example" in {
    object Test extends Z3 {
      def test(): Boolean = {
        init()
        val intSort = z3.mkIntSort

        val x: Z3AST = z3.mkConst(z3.mkStringSymbol("x"), intSort)
        val y: Z3AST = z3.mkConst(z3.mkStringSymbol("y"), intSort)
        val z: Z3AST = z3.mkConst(z3.mkStringSymbol("z"), intSort)
        val c0 = z3.mkInt(0, intSort)
        val c1 = z3.mkInt(1, intSort)
        val c2 = z3.mkInt(2, intSort)
        val c3 = z3.mkInt(3, intSort)
        val cm1 = z3.mkInt(-1, intSort)

        val cs1 = z3.mkGT(x, c0)
        val cs2 = z3.mkLE(x, cm1)
        val cs3 = z3.mkOr(z3.mkGT(x, c0), z3.mkLT(y, c1))
        val cs4 = z3.mkGT(y, c2)
        val cs5 = z3.mkGT(y, c3)
        val cs6 = z3.mkLE(y, cm1)
        val cs7 = z3.mkEq(z, z3.mkAdd(x, y))

        solver.assertCnstr(z3.mkAnd(cs1, cs2, cs3, cs4, cs5, cs6, cs7))
        val Some(result) = solver.check()
        delete()
        result
      }
    }
    Test.test() should be (false)
  }

}
