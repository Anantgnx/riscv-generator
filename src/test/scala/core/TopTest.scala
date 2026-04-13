package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TopTest extends AnyFlatSpec with ChiselScalatestTester {

  // Increase per-benchmark if needed, but keep low to avoid 48min runs
  val maxCycles = 3000

  def runBench(benchId: Int, expected: BigInt, name: String, desc: String): Unit = {
    val cfg = Config(benchmark = benchId, hasMul = true, memLatency = 10)

    println("=" * 70)
    println(s"Benchmark $benchId: $name — $desc")
    println("=" * 70)

    // Collect trace in memory, only print on failure

    test(new Top(cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      var cycles    = 0
      var done      = false
      var lastPc    = BigInt(-1)
      var haltCount = 0

      println(f"${"Cyc"}%5s | ${"PC"}%6s | ${"x10"}%4s ${"x11"}%4s ${"x12"}%4s ${"x13"}%4s ${"x14"}%4s ${"x15"}%4s ${"x16"}%4s ${"x17"}%4s | ${"a0"}%3s ${"a1"}%3s ${"a2"}%3s ${"a3"}%3s ${"a4"}%3s ${"a5"}%3s | ${"DC"}%6s ${"hb"}%2s ${"hz"}%2s")
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
        val a0   = dut.io.debug_arr0.peek().litValue
        val a1   = dut.io.debug_arr1.peek().litValue
        val a2   = dut.io.debug_arr2.peek().litValue
        val a3   = dut.io.debug_arr3.peek().litValue
        val a4   = dut.io.debug_arr4.peek().litValue
        val a5   = dut.io.debug_arr5.peek().litValue
        val dcSt = dut.io.debug_dcache_state.peek().litValue.toInt
        val dcStr = if (dcSt == 0) "IDLE  " else "REFILL"
        val hb   = dut.io.debug_hb.peek().litValue
        val hz   = dut.io.debug_hz.peek().litValue

        println(f"$cycles%5d | 0x$pc%04x | $x10%4d $x11%4d $x12%4d $x13%4d $x14%4d $x15%4d $x16%4d $x17%4d | $a0%3d $a1%3d $a2%3d $a3%3d $a4%3d $a5%3d | $dcStr $hb%2d $hz%2d")

        if (x10 == expected) {
          done = true
          println(s"[DONE] x10=$x10 at cycle $cycles")
        } else if (pc == lastPc && pc > 0x7C) {
          haltCount += 1
          if (haltCount >= 5) {
            println(s"[HALT] PC stuck at 0x${pc.toString(16)} after $cycles cycles")
            done = true
          }
        } else { haltCount = 0 }
        lastPc = pc

        if (!done) { dut.clock.step(1); cycles += 1 }
      }

      val x10    = dut.io.debug_x10.peek().litValue
      val hits   = dut.io.debug_hits.peek().litValue
      val misses = dut.io.debug_misses.peek().litValue
      val total  = hits + misses
      val hr     = if (total > 0) hits.toDouble / total.toDouble * 100.0 else 0.0



      val passed = x10 == expected
      println(f"x10=$x10 (expected $expected) | cycles=$cycles | hits=$hits misses=$misses hr=$hr%.1f%%")

      if (benchId == 2) {
        val a0 = dut.io.debug_arr0.peek().litValue
        val a1 = dut.io.debug_arr1.peek().litValue
        val a2 = dut.io.debug_arr2.peek().litValue
        val a3 = dut.io.debug_arr3.peek().litValue
        val a4 = dut.io.debug_arr4.peek().litValue
        val a5 = dut.io.debug_arr5.peek().litValue
        println(f"Final arr (from cache) = [$a0, $a1, $a2, $a3, $a4, $a5]")
      }

      assert(passed, s"$name FAILED: x10=$x10 expected $expected")
      println(s"$name PASSED\n")
    }
  }

  "Benchmark 0: 2x2 MatMul" should "produce x10=50" in {
    runBench(0, BigInt(50), "2x2 MatMul", "A=[[1,2],[3,4]] B=[[5,6],[7,8]] → C[1][1]=50")
  }

  "Benchmark 1: 4x4 MatMul" should "produce x10=16" in {
    runBench(1, BigInt(16), "4x4 MatMul", "A=[[1..16]] B=identity → C[3][3]=16")
  }

  "Benchmark 2: BubbleSort" should "produce x10=1" in {
    runBench(2, BigInt(1), "BubbleSort", "arr=[8,7,6,5,4,3,2,1] → arr[0]=1")
  }

  "Benchmark 3: Histogram" should "produce x10=4" in {
    runBench(3, BigInt(4), "Histogram", "32 inputs [0..7]x4 → bins[0]=4")
  }
}