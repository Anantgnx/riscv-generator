package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Tier1Test extends AnyFlatSpec with ChiselScalatestTester {

  val maxCycles = 200

  "Tier1Test" should "verify all new Tier1 ISA instructions" in {
    val cfg = Config(benchmark = 5, hasMul = true, memLatency = 1)

    println("=" * 65)
    println("Tier 1 ISA Test — XOR OR AND SLL SRL SRA SUB SLTU SLLI SRLI")
    println("Chain: x10=16 only if every instruction produces correct result")
    println("=" * 65)
    println(f"${"Cyc"}%4s | ${"PC"}%6s | ${"x10"}%4s ${"x11"}%4s ${"x12"}%4s ${"x13"}%4s ${"x14"}%4s ${"x15"}%4s ${"x16"}%4s")

    test(new Top(cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      var cycles = 0
      var done   = false

      while (cycles < maxCycles && !done) {
        val pc  = dut.io.debug_pc.peek().litValue
        val x10 = dut.io.debug_x10.peek().litValue
        val x11 = dut.io.debug_x11.peek().litValue
        val x12 = dut.io.debug_x12.peek().litValue
        val x13 = dut.io.debug_x13.peek().litValue
        val x14 = dut.io.debug_x14.peek().litValue
        val x15 = dut.io.debug_x15.peek().litValue
        val x16 = dut.io.debug_x16.peek().litValue

        println(f"$cycles%4d | 0x$pc%04x | $x10%4d $x11%4d $x12%4d $x13%4d $x14%4d $x15%4d $x16%4d")

        if (x10 == 16) { println(s"[DONE] x10=16 at cycle $cycles"); done = true }
        if (!done) { dut.clock.step(1); cycles += 1 }
      }

      println()
      println("--- FINAL REGISTER CHECK ---")

      val x10 = dut.io.debug_x10.peek().litValue
      val x11 = dut.io.debug_x11.peek().litValue
      val x12 = dut.io.debug_x12.peek().litValue
      val x13 = dut.io.debug_x13.peek().litValue
      val x14 = dut.io.debug_x14.peek().litValue
      val x15 = dut.io.debug_x15.peek().litValue
      val x16 = dut.io.debug_x16.peek().litValue

      // Print each intermediate result with pass/fail
      def check(name: String, got: BigInt, exp: BigInt): Unit = {
        val ok = if (got == exp) "PASS" else s"FAIL (got $got expected $exp)"
        println(f"  $name%-30s $ok")
      }

      check("x11 (operand A=15)",      x11, 15)
      check("x12 (shift amt=2)",        x12, 2)
      check("x15 (operand B=6)",        x15, 6)
      check("x13 after SRLI (=15)",     x13, 15)
      check("x14 after SRA  (=-15)",    x14, BigInt("FFFFFFF1", 16))  // -15 as unsigned 32-bit
      check("x16 after SLTU (=1)",      x16, 1)
      check("x10 final (=16)",          x10, 16)

      println()
      assert(x10 == 16, s"Tier 1 FAILED: x10=$x10 expected 16")
      println("Tier 1 ISA Test PASSED — all new instructions working correctly")
    }
  }
}