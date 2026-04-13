package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Tier3Part2Test extends AnyFlatSpec with ChiselScalatestTester {

  val maxCycles = 500

  "Tier3Part2Test" should "verify LB LBU LH LHU SB SH byte/halfword ops" in {
    val cfg = Config(benchmark = 8, hasMul = true, memLatency = 1)

    println("=" * 65)
    println("Tier 3 Part 2 — LB / LBU / LH / LHU / SB / SH")
    println("SW 0xAABBCCDD → 0x080")
    println("SB 0x11 → byte1  → expect 0xAABB11DD")
    println("SH 0x55 → half1  → expect 0x005511DD")
    println("Expected: x10=39523, x15=0xAABB11DD, x16=0x005511DD")
    println("=" * 65)
    println(f"${"Cyc"}%4s | ${"PC"}%6s | ${"x10"}%10s ${"x11"}%5s ${"x12"}%5s ${"x13"}%7s ${"x14"}%6s ${"x15"}%12s ${"x16"}%12s")

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

        val x10s = x10.toLong - (if (x10 > 0x7FFFFFFFL) 0x100000000L else 0)
        val x12s = x12.toLong - (if (x12 > 0x7FFFFFFFL) 0x100000000L else 0)
        val x13s = x13.toLong - (if (x13 > 0x7FFFFFFFL) 0x100000000L else 0)

        println(f"$cycles%4d | 0x$pc%04x | $x10s%10d $x11%5d $x12s%5d $x13s%7d $x14%6d 0x$x15%08x 0x$x16%08x")

        if (x10 == 39523) { println(s"[DONE] x10=39523 at cycle $cycles"); done = true }
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

      def check(name: String, got: BigInt, exp: BigInt): Unit =
        println(f"  $name%-40s ${if (got==exp) "PASS" else s"FAIL (got 0x${got.toString(16)} expected 0x${exp.toString(16)})"}")

      check("x11 LBU 0(0x080) = 221",          x11, 221)
      check("x12 LB  1(0x080) = 0xFFFFFFCC",   x12, BigInt("FFFFFFCC", 16))
      check("x13 LH  0(0x080) = 0xFFFFCCDD",   x13, BigInt("FFFFCCDD", 16))
      check("x14 LHU 0(0x080) = 52445",         x14, 52445)
      check("x15 LW  after SB = 0xAABB11DD",   x15, BigInt("AABB11DD", 16))
      check("x16 LW  after SH = 0x005511DD",   x16, BigInt("005511DD", 16))
      check("x10 checksum     = 39523",         x10, 39523)

      assert(x15 == BigInt("AABB11DD", 16), s"SB FAILED")
      assert(x16 == BigInt("005511DD", 16), s"SH FAILED")
      assert(x10 == 39523, s"checksum FAILED")
      println("\nTier 3 Part 2 PASSED — all byte/halfword ops correct including SH")
    }
  }
}