package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TopTest extends AnyFlatSpec with ChiselScalatestTester {

  val cfg = Config(benchmark = 0, hasMul = true)
  val maxCycles = 500

  "RISCV Processor" should "run benchmark" in {
    test(new Top(cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      println("=" * 70)
      println("2x2 Matrix Multiply: C = A * B")
      println("A=[[1,2],[3,4]]  B=[[5,6],[7,8]]")
      println("Expected: C=[[19,22],[43,50]], x10=C[1][1]=50")
      println("=" * 70)
      println(f"${"Cyc"}%5s | ${"PC"}%6s | ${"x10"}%4s ${"x11"}%4s ${"x12"}%4s ${"x13"}%4s ${"x14"}%4s ${"x15"}%4s ${"x16"}%4s ${"x17"}%4s ${"x18"}%5s ${"x19"}%5s ${"x20"}%5s ${"x21"}%5s ${"x22"}%5s ${"x23"}%5s ${"x24"}%5s | ${"DC"}%6s ${"hb"}%2s ${"hz"}%2s | ${"c11"}%4s")
      println("-" * 70)

      var cycles    = 0
      var lastPc    = BigInt(-1)
      var haltCount = 0
      var done      = false

      while (cycles < maxCycles && !done) {
        val pc   = dut.io.debug_pc.peek().litValue
        val x10  = dut.io.debug_x10.peek().litValue
        val x11  = dut.io.debug_x11.peek().litValue
        val x12  = dut.io.debug_x12.peek().litValue
        val x13  = dut.io.debug_x13.peek().litValue
        val x14  = dut.io.debug_x14.peek().litValue
        val x15  = dut.io.debug_x15.peek().litValue
        val x16  = dut.io.debug_x16.peek().litValue
        val x17  = dut.io.debug_x17.peek().litValue
        val x18  = dut.io.debug_x18.peek().litValue
        val x19  = dut.io.debug_x19.peek().litValue
        val x20  = dut.io.debug_x20.peek().litValue
        val x21  = dut.io.debug_x21.peek().litValue
        val x22  = dut.io.debug_x22.peek().litValue
        val x23  = dut.io.debug_x23.peek().litValue
        val x24  = dut.io.debug_x24.peek().litValue
        val dcSt = dut.io.debug_dcache_state.peek().litValue.toInt
        val dcStr = if (dcSt == 0) "IDLE  " else "REFILL"
        val hb   = dut.io.debug_hb.peek().litValue
        val hz   = dut.io.debug_hz.peek().litValue
        val dc11 = dut.io.debug_c11.peek().litValue

        println(f"$cycles%5d | 0x$pc%04x | $x10%4d $x11%4d $x12%4d $x13%4d $x14%4d $x15%4d $x16%4d $x17%4d $x18%5d $x19%5d $x20%5d $x21%5d $x22%5d $x23%5d $x24%5d | $dcStr $hb%2d $hz%2d | $dc11%4d")

        // Exit as soon as x10=50
        if (x10 == 50) {
          println(s"\n[DONE] x10=50 reached at cycle $cycles")
          done = true
        }

        // Safety: exit if PC stuck past end of program
        if (!done) {
          if (pc == lastPc && pc > 0x7C) {
            haltCount += 1
            if (haltCount >= 5) {
              println(s"\n[HALT] PC 0x${pc.toString(16)} stuck — pipeline may be hung")
              done = true
            }
          } else { haltCount = 0 }
          lastPc = pc
          dut.clock.step(1)
          cycles += 1
        }
      }

      // Read final values
      val x10 = dut.io.debug_x10.peek().litValue
      val c00 = dut.io.debug_c00.peek().litValue
      val c01 = dut.io.debug_c01.peek().litValue
      val c10 = dut.io.debug_c10.peek().litValue
      val c11 = dut.io.debug_c11.peek().litValue
      val hits   = dut.io.debug_hits.peek().litValue
      val misses = dut.io.debug_misses.peek().litValue
      val total  = hits + misses
      val hr     = if (total > 0) hits.toDouble / total.toDouble * 100.0 else 0.0

      println("=" * 70)
      println("Result matrix C = A * B:")
      println(f"  | $c00%4d  $c01%4d |")
      println(f"  | $c10%4d  $c11%4d |")
      println(f"  Expected: [[19,22],[43,50]]")
      println(f"Final x10    : $x10  (expected 50)")
      println(f"Cache hits   : $hits")
      println(f"Cache misses : $misses")
      println(f"Hit rate     : $hr%.1f%%")
      println("=" * 70)

      assert(x10 == 50, s"x10 expected 50 got $x10")
      println("MATMUL TEST PASSED")
    }
  }
}