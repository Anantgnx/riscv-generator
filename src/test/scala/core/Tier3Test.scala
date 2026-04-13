package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Tier3Test extends AnyFlatSpec with ChiselScalatestTester {

  val maxCycles = 200

  "Tier3Test" should "verify JAL and JALR (function call and return)" in {
    val cfg = Config(benchmark = 7, hasMul = true, memLatency = 1)

    println("=" * 60)
    println("Tier 3 ISA Test — JAL / JALR")
    println("main: addi x11,5 → JAL func@0x18 → after return: x10+=100")
    println("func: x10=x11+10=15 → JALR x0,x1,0 (return to 0x0C)")
    println("Expected: x10=115, x1(ra)=12(0x0C), x11=5")
    println("=" * 60)
    println(f"${"Cyc"}%4s | ${"PC"}%6s | ${"x1(ra)"}%7s ${"x10"}%5s ${"x11"}%4s")

    test(new Top(cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      var cycles = 0
      var done   = false

      while (cycles < maxCycles && !done) {
        val pc  = dut.io.debug_pc.peek().litValue
        val x1  = dut.io.debug_x1.peek().litValue
        val x10 = dut.io.debug_x10.peek().litValue
        val x11 = dut.io.debug_x11.peek().litValue

        println(f"$cycles%4d | 0x$pc%04x | $x1%7d $x10%5d $x11%4d")

        if (x10 == 115) { println(s"[DONE] x10=115 at cycle $cycles"); done = true }
        if (!done) { dut.clock.step(1); cycles += 1 }
      }

      println()
      println("--- FINAL REGISTER CHECK ---")
      val x1  = dut.io.debug_x1.peek().litValue
      val x10 = dut.io.debug_x10.peek().litValue
      val x11 = dut.io.debug_x11.peek().litValue

      def check(name: String, got: BigInt, exp: BigInt): Unit =
        println(f"  $name%-35s ${if (got==exp) "PASS" else s"FAIL (got $got expected $exp)"}")

      check("x1  ra after JAL (=12)",   x1,  12)   // 0x0C = 12
      check("x11 arg (=5)",             x11, 5)
      check("x10 final (=115)",         x10, 115)

      assert(x1  == 12,  s"JAL  FAILED: x1=$x1 expected 12 (return addr 0x0C)")
      assert(x10 == 115, s"JALR FAILED: x10=$x10 expected 115")
      println("\nTier 3 JAL/JALR Test PASSED")
    }
  }
}