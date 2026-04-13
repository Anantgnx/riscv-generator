package core

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class DhrystoneTest extends AnyFlatSpec with ChiselScalatestTester {

  "DhrystoneTest" should "run Dhrystone and reach ebreak/halt" in {
    val cfg = Config(
      benchmark  = 9,
      hexFile    = "sw/picorv32/dhrystone/dhry_imem.hex",
      hasMul     = true,
      memLatency = 1
    )

    println("=" * 60)
    println("Dhrystone Benchmark on Custom RV32IM Core")
    println("Running with Verilator backend")
    println("=" * 60)

    test(new Top(cfg)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      var cycles    = 0
      var lastPc    = BigInt(-1)
      var haltCount = 0
      var done      = false
      val maxCycles = 50000000

      val startTime = System.currentTimeMillis()

      while (cycles < maxCycles && !done) {
        val pc = dut.io.debug_pc.peek().litValue

        if (cycles % 1000000 == 0) {
          val elapsed = (System.currentTimeMillis() - startTime) / 1000.0
          val x10 = dut.io.debug_x10.peek().litValue
          println(f"  cyc=$cycles%9d  PC=0x$pc%08x  x10=$x10%d  elapsed=${elapsed}%.1fs")
        }

        if (pc == lastPc && cycles > 10) {
          haltCount += 1
          if (haltCount >= 50) {
            println(s"\n[HALT] PC stuck at 0x${pc.toString(16)} after $cycles cycles")
            done = true
          }
        } else {
          haltCount = 0
        }
        lastPc = pc

        if (!done) { dut.clock.step(1); cycles += 1 }
      }

      val elapsed = (System.currentTimeMillis() - startTime) / 1000.0
      println(s"\nFinished: $cycles cycles in ${elapsed}s")
      println(f"Simulation speed: ${cycles / elapsed / 1000000.0}%.2f Mcycles/sec")
    }
  }
}