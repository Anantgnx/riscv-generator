package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class WriteThroughTest extends AnyFlatSpec with ChiselScalatestTester {

  "WriteThroughTest" should "reflect write-hits in DataRAM immediately" in {

    val cfg = Config(benchmark = 4, hasMul = true, memLatency = 10)

    println("=" * 70)
    println("Write-Through Test")
    println("Program: lw arr[0], lw arr[1], sw 99→arr[0], sw 42→arr[1], lw arr[0]")
    println("Expected: x10=99, cache[0]=99, cache[1]=42, DataRAM[0]=99, DataRAM[1]=42")
    println("=" * 70)
    println(f"${"Cyc"}%5s | ${"PC"}%6s | ${"x10"}%4s ${"x11"}%4s ${"x12"}%4s ${"x13"}%4s ${"x14"}%4s | " +
      f"${"C[0]"}%5s ${"C[1]"}%5s | ${"D[0]"}%5s ${"D[1]"}%5s | ${"DC"}%6s ${"hb"}%2s ${"hz"}%2s")

    test(new Top(cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      var cycles = 0
      var done   = false

      while (cycles < 500 && !done) {
        val pc  = dut.io.debug_pc.peek().litValue
        val x10 = dut.io.debug_x10.peek().litValue
        val x11 = dut.io.debug_x11.peek().litValue
        val x12 = dut.io.debug_x12.peek().litValue
        val x13 = dut.io.debug_x13.peek().litValue
        val x14 = dut.io.debug_x14.peek().litValue

        // Cache values (live, from cache data array)
        val c0  = dut.io.debug_arr0.peek().litValue
        val c1  = dut.io.debug_arr1.peek().litValue

        // DataRAM values (from DataRAM register file directly)
        val d0  = dut.io.debug_dmem0.peek().litValue
        val d1  = dut.io.debug_dmem1.peek().litValue

        val dcSt  = dut.io.debug_dcache_state.peek().litValue.toInt
        val dcStr = if (dcSt == 0) "IDLE  " else "REFILL"
        val hb    = dut.io.debug_hb.peek().litValue
        val hz    = dut.io.debug_hz.peek().litValue

        println(f"$cycles%5d | 0x$pc%04x | $x10%4d $x11%4d $x12%4d $x13%4d $x14%4d | " +
          f"$c0%5d $c1%5d | $d0%5d $d1%5d | $dcStr $hb%2d $hz%2d")

        if (x10 == 99) {
          println(s"[DONE] x10=99 at cycle $cycles")
          done = true
        }

        if (!done) { dut.clock.step(1); cycles += 1 }
      }

      val x10 = dut.io.debug_x10.peek().litValue
      val c0  = dut.io.debug_arr0.peek().litValue
      val c1  = dut.io.debug_arr1.peek().litValue
      val d0  = dut.io.debug_dmem0.peek().litValue
      val d1  = dut.io.debug_dmem1.peek().litValue

      println(s"\n--- FINAL STATE ---")
      println(f"x10 (cache read-back) = $x10  (expected 99)")
      println(f"Cache  arr[0]=$c0  arr[1]=$c1  (expected 99, 42)")
      println(f"DataRAM arr[0]=$d0  arr[1]=$d1  (expected 99, 42)")

      val cacheOk  = c0 == 99 && c1 == 42
      val dramOk   = d0 == 99 && d1 == 42
      val readOk   = x10 == 99

      println(s"\nCache  write-through: ${if (cacheOk) "PASS" else "FAIL"}")
      println(s"DataRAM write-through: ${if (dramOk)  "PASS ✓" else "FAIL ✗ (bug: write-hit not reaching DataRAM)"}")
      println(s"Cache  read-back:      ${if (readOk)  "PASS" else "FAIL"}")

      assert(readOk,  s"x10=$x10 expected 99 — cache read-back broken")
      assert(cacheOk, s"cache arr[0]=$c0 arr[1]=$c1 expected 99,42")
      assert(dramOk,  s"DataRAM arr[0]=$d0 arr[1]=$d1 expected 99,42 — write-through bug!")
    }
  }
}