/*package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SweepTest extends AnyFlatSpec with ChiselScalatestTester {

  val maxCycles = 10000

  val memLatencies = Seq(1, 2, 5, 10, 20, 50)

  // Expected result and name per benchmark
  val benchmarks = Seq(
    (0, BigInt(50),  "MatMul    "),
    (1, BigInt(272), "VecReduce "),
    (2, BigInt(1),   "BubbleSort"),
    (3, BigInt(4),   "Histogram ")
  )

  def runOne(cfg: Config, expectedX10: BigInt): (Int, BigInt, BigInt, Boolean) = {
    var cycles    = 0
    var hits      = BigInt(0)
    var misses    = BigInt(0)
    var passed    = false
    var done      = false
    var lastPc    = BigInt(-1)
    var haltCount = 0

    test(new Top(cfg)) { dut =>
      dut.clock.setTimeout(0)
      while (cycles < maxCycles && !done) {
        val x10 = dut.io.debug_x10.peek().litValue
        val pc  = dut.io.debug_pc.peek().litValue

        if (x10 == expectedX10) {
          passed = true
          done   = true
        } else if (pc == lastPc && pc > 0x7C) {
          haltCount += 1
          if (haltCount >= 5) done = true
        } else {
          haltCount = 0
        }
        lastPc = pc

        if (!done) {
          dut.clock.step(1)
          cycles += 1
        }
      }
      hits   = dut.io.debug_hits.peek().litValue
      misses = dut.io.debug_misses.peek().litValue
    }
    (cycles, hits, misses, passed)
  }

  "Design Space Sweep" should "explore memLatency x benchmark" in {
    val sep = "=" * 82
    println("\n" + sep)
    println(f"${"benchmark"}%10s | ${"latency"}%7s | ${"cycles"}%7s | ${"hits"}%5s | ${"misses"}%6s | ${"hitRate%"}%8s | ${"pass"}%4s")
    println(sep)

    for ((benchId, expected, name) <- benchmarks) {
      println("-" * 82)
      for (lat <- memLatencies) {
        val cfg = Config(
          benchmark          = benchId,
          hasMul             = true,
          cacheSizeKB        = 1,
          cacheAssociativity = 1,
          memLatency         = lat
        )
        val (cycles, hits, misses, passed) = runOne(cfg, expected)
        val total   = hits + misses
        val hr      = if (total > 0) hits.toDouble / total.toDouble * 100.0 else 0.0
        val passStr = if (passed) "PASS" else "FAIL"
        println(f"$name%10s | $lat%7d | $cycles%7d | $hits%5d | $misses%6d | $hr%8.1f | $passStr%4s")
      }
    }

    println(sep)
    println("Sweep complete.")
  }
}
*/