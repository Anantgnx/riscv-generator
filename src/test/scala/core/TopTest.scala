package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TopTest extends AnyFlatSpec with ChiselScalatestTester {

  val cfg = Config(benchmark = 0, hasMul = true)
  val maxCycles = 5000

  "RISCV Processor" should "run benchmark" in {
    test(new Top(cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      println("=" * 100)
      println("2x2 Matrix Multiply: C = A * B")
      println("A=[[1,2],[3,4]]  B=[[5,6],[7,8]]")
      println("Expected: C=[[19,22],[43,50]], x10=C[1][1]=50")
      println("=" * 100)
      println(f"${"Cyc"}%5s | ${"PC"}%6s | ${"x10"}%4s ${"x11"}%4s ${"x12"}%4s ${"x13"}%4s ${"x14"}%4s ${"x15"}%4s ${"x16"}%4s ${"x17"}%4s ${"x18"}%5s ${"x19"}%5s ${"x20"}%5s ${"x21"}%5s ${"x22"}%5s ${"x23"}%5s ${"x24"}%5s | ${"DC"}%6s ${"hb"}%2s ${"hz"}%2s")
      println("-" * 100)

      var cycles = 0
      var lastPc = BigInt(-1)
      var haltCount = 0

      while (cycles < maxCycles) {
        val pc    = dut.io.debug_pc.peek().litValue
        val x10   = dut.io.debug_x10.peek().litValue
        val x11   = dut.io.debug_x11.peek().litValue
        val x12   = dut.io.debug_x12.peek().litValue
        val x13   = dut.io.debug_x13.peek().litValue
        val x14   = dut.io.debug_x14.peek().litValue
        val x15   = dut.io.debug_x15.peek().litValue
        val x16   = dut.io.debug_x16.peek().litValue
        val x17   = dut.io.debug_x17.peek().litValue
        val x18   = dut.io.debug_x18.peek().litValue
        val x19   = dut.io.debug_x19.peek().litValue
        val x20   = dut.io.debug_x20.peek().litValue
        val x21   = dut.io.debug_x21.peek().litValue
        val x22   = dut.io.debug_x22.peek().litValue
        val x23   = dut.io.debug_x23.peek().litValue
        val x24   = dut.io.debug_x24.peek().litValue
        val dcSt  = dut.io.debug_dcache_state.peek().litValue.toInt
        val dcStr = if (dcSt == 0) "IDLE  " else "REFILL"
        val hb    = dut.io.debug_hb.peek().litValue
        val hz    = dut.io.debug_hz.peek().litValue

        println(f"$cycles%5d | 0x$pc%04x | $x10%4d $x11%4d $x12%4d $x13%4d $x14%4d $x15%4d $x16%4d $x17%4d $x18%5d $x19%5d $x20%5d $x21%5d $x22%5d $x23%5d $x24%5d | $dcStr $hb%2d $hz%2d")

        // Detect NOP loop (PC stuck past end of program = 0x7C)
        if (pc == lastPc && pc > 0x7C) {
          haltCount += 1
          if (haltCount >= 5) {
            println(s"\n[HALT] PC 0x${pc.toString(16)} stable for 5 cycles — program complete or hung")
            dut.clock.step(1)
            cycles += 1
            // advance pipeline so WB can drain
            for (_ <- 0 until 8) { dut.clock.step(1); cycles += 1 }
            // re-read x10 after drain
            val finalX10 = dut.io.debug_x10.peek().litValue
            println(s"[HALT] x10 after drain = $finalX10")
            cycles = maxCycles  // break
          }
        } else {
          haltCount = 0
        }
        lastPc = pc

        dut.clock.step(1)
        cycles += 1
      }

      val x10    = dut.io.debug_x10.peek().litValue
      val hits   = dut.io.debug_hits.peek().litValue
      val misses = dut.io.debug_misses.peek().litValue
      val total  = hits + misses
      val hr     = if (total > 0) hits.toDouble / total.toDouble * 100.0 else 0.0

      println("=" * 100)
      println(f"Final x10    : $x10  (expected 50)")
      println(f"Cache hits   : $hits")
      println(f"Cache misses : $misses")
      println(f"Hit rate     : $hr%.1f%%")
      println("=" * 100)

      assert(x10 == 50, s"x10 expected 50 got $x10")
      println("MATMUL TEST PASSED")
    }
  }
}