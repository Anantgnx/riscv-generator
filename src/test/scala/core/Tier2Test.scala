package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Tier2Test extends AnyFlatSpec with ChiselScalatestTester {

  val maxCycles = 200

  "Tier2Test" should "verify LUI, AUIPC, BLT, BGE, BLTU, BGEU" in {
    val cfg = Config(benchmark = 6, hasMul = true, memLatency = 1)

    println("=" * 65)
    println("Tier 2 ISA Test — LUI  AUIPC  BLT  BGE  BLTU  BGEU")
    println("Expected x10=4106 = 4096(LUI)+1(BLT)+2(BGE)+3(BLTU)+4(BGEU)")
    println("=" * 65)
    println(f"${"Cyc"}%4s | ${"PC"}%6s | ${"x10"}%5s ${"x11"}%5s ${"x12"}%5s ${"x13"}%4s ${"x14"}%4s ${"x15"}%4s ${"x16"}%4s ${"x17"}%4s ${"x18"}%4s")

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
        val x17 = dut.io.debug_x17.peek().litValue
        val x18 = dut.io.debug_x18.peek().litValue

        println(f"$cycles%4d | 0x$pc%04x | $x10%5d $x11%5d $x12%5d $x13%4d $x14%4d $x15%4d $x16%4d $x17%4d $x18%4d")

        if (x10 == 4106) { println(s"[DONE] x10=4106 at cycle $cycles"); done = true }
        if (!done) { dut.clock.step(1); cycles += 1 }
      }

      println()
      println("--- FINAL REGISTER CHECK ---")
      val x10 = dut.io.debug_x10.peek().litValue
      val x11 = dut.io.debug_x11.peek().litValue
      val x12 = dut.io.debug_x12.peek().litValue
      val x15 = dut.io.debug_x15.peek().litValue
      val x16 = dut.io.debug_x16.peek().litValue
      val x17 = dut.io.debug_x17.peek().litValue
      val x18 = dut.io.debug_x18.peek().litValue

      def check(name: String, got: BigInt, exp: BigInt): Unit =
        println(f"  $name%-35s ${if (got==exp) "PASS" else s"FAIL (got $got expected $exp)"}")

      check("x11 LUI  (=4096)",       x11, 4096)
      check("x12 AUIPC (=4, PC@0x04)",x12, 4)
      check("x15 BLT  taken (=1)",    x15, 1)
      check("x16 BGE  taken (=2)",    x16, 2)
      check("x17 BLTU taken (=3)",    x17, 3)
      check("x18 BGEU taken (=4)",    x18, 4)
      check("x10 final (=4106)",      x10, 4106)

      assert(x10 == 4106, s"Tier 2 FAILED: x10=$x10 expected 4106")
      println("\nTier 2 ISA Test PASSED")
    }
  }
}