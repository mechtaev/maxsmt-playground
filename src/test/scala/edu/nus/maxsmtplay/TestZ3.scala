package edu.nus.maxsmtplay

import com.microsoft.z3._
import org.scalatest._

class TestZ3 extends FlatSpec with Matchers {

  "Z3 solver" should "get UNSAT answer for the Z3 MaxSAT example" in {
    object Test extends Z3 {
      def test(): Boolean = {
        init(None)

        val x: IntExpr = z3.mkIntConst("x")
        val y: IntExpr = z3.mkIntConst("y")
        val z: IntExpr = z3.mkIntConst("z")
        val c0 = z3.mkInt(0)
        val c1 = z3.mkInt(1)
        val c2 = z3.mkInt(2)
        val c3 = z3.mkInt(3)
        val cm1 = z3.mkInt(-1)

        val cs1 = z3.mkGt(x, c0)
        val cs2 = z3.mkLe(x, cm1)
        val cs3 = z3.mkOr(z3.mkGt(x, c0), z3.mkLt(y, c1))
        val cs4 = z3.mkGt(y, c2)
        val cs5 = z3.mkGt(y, c3)
        val cs6 = z3.mkLe(y, cm1)
        val cs7 = z3.mkEq(z, z3.mkAdd(x, y))

        solver.add(z3.mkAnd(cs1, cs2, cs3, cs4, cs5, cs6, cs7))
        val result = solver.check().toInt() > -1
        delete()
        result
      }
    }
    Test.test() should be (false)
  }

}
