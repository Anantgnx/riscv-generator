package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

// ============================================================
// Design Space Sweep — cacheSizeKB x cacheAssociativity x bench1
//
// bench1 (4x4 MatMul) has the largest working set (48 words=192B).
// Even so, it fits comfortably in 1KB direct-mapped with no conflicts,
// so hit rate is invariant to cache geometry across all tested configs.
// The sweep confirms this and reports cycles/CPI as a function of geometry.
// memLatency fixed at 10 throughout.
// ============================================================
class SweepTest extends AnyFlatSpec with ChiselScalatestTester {

  val maxCycles   = 20000
  val approxInsts = 550  // approximate instruction count for bench1

  val cacheSizes = Seq(1, 2, 4)   // cacheSizeKB
  val assocs     = Seq(1, 2, 4)   // cacheAssociativity

  def runOne(cfg: Config, expected: BigInt): (Int, BigInt, BigInt, Boolean) = {
    var cycles = 0; var hits = BigInt(0); var misses = BigInt(0)
    var passed = false; var done = false
    var lastPc = BigInt(-1); var haltCount = 0
    test(new Top(cfg)) { dut =>
      dut.clock.setTimeout(0)
      while (cycles < maxCycles && !done) {
        val x10 = dut.io.debug_x10.peek().litValue
        val pc  = dut.io.debug_pc.peek().litValue
        if (x10 == expected) { passed = true; done = true }
        else if (pc == lastPc && pc > 0x7C) {
          haltCount += 1
          if (haltCount >= 5) done = true
        } else haltCount = 0
        lastPc = pc
        if (!done) { dut.clock.step(1); cycles += 1 }
      }
      hits   = dut.io.debug_hits.peek().litValue
      misses = dut.io.debug_misses.peek().litValue
    }
    (cycles, hits, misses, passed)
  }

  "SweepTest" should "sweep cacheSizeKB x cacheAssociativity for bench1" in {
    val sep  = "=" * 75
    val dash = "-" * 75

    println("\n" + sep)
    println("SWEEP — bench1 (4x4 MatMul) | cacheSizeKB x assoc | memLatency=10")
    println(sep)
    println(f"${"cacheKB"}%7s | ${"assoc"}%5s | ${"numSets"}%7s | ${"cycles"}%7s | ${"hits"}%5s | ${"misses"}%6s | ${"hit%"}%6s | ${"CPI"}%5s | ${"pass"}%4s")
    println(sep)

    for (kb <- cacheSizes) {
      println(dash)
      for (assoc <- assocs) {
        val numSets = (kb * 1024) / 4 / assoc
        val cfg = Config(
          benchmark          = 1,
          hasMul             = true,
          cacheSizeKB        = kb,
          cacheAssociativity = assoc,
          memLatency         = 10
        )
        val (cycles, hits, misses, passed) = runOne(cfg, BigInt(16))
        val total = hits + misses
        val hr    = if (total > 0) hits.toDouble / total.toDouble * 100.0 else 0.0
        val cpi   = cycles.toDouble / approxInsts.toDouble
        val pass  = if (passed) "PASS" else "FAIL"
        println(f"$kb%7d | $assoc%5d | $numSets%7d | $cycles%7d | $hits%5d | $misses%6d | $hr%6.1f | $cpi%5.2f | $pass%4s")
      }
    }

    println(sep)
    println("Sweep complete.")
  }
}