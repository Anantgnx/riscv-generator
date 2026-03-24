package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TopTest extends AnyFlatSpec with ChiselScalatestTester {
  "RISCV Processor" should "handle simultaneous Cache misses and Arbitration" in {
    // Passing the default Config; you can toggle isThreeStage here if needed
    test(new Top(Config())).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      println("\n" + "="*60)
      println("RISC-V System-on-Chip Simulation: Logic Trace")
      println("="*60)

      for (cycle <- 0 until 100) {
        // We use .peekInt().toInt to convert BigInt to a standard Integer for the 'match'
        val pc     = dut.io.debug_pc.peekInt().toInt
        val inst   = dut.io.debug_inst.peekInt().toLong
        val iState = dut.io.debug_icache_state.peekInt().toInt
        val dState = dut.io.debug_dcache_state.peekInt().toInt

        // Map State Integers to Names for clear terminal debugging
        val iName = iState match {
          case 0 => "IDLE   "
          case 1 => "LOOKUP "
          case 2 => "COMPARE"
          case 3 => "REFILL "
          case _ => "UNKNOWN"
        }

        val dName = dState match {
          case 0 => "IDLE   "
          case 1 => "LOOKUP "
          case 2 => "COMPARE"
          case 3 => "REFILL "
          case _ => "UNKNOWN"
        }

        // Print the hardware status for this clock cycle
        println(f"Cycle $cycle%2d | PC: 0x$pc%04x | Inst: 0x$inst%08x | I-Cache: $iName | D-Cache: $dName")

        dut.clock.step(1)
      }

      println("="*60)
      println("Simulation Finished. Check test_run_dir/ for the VCD file.")
      println("="*60)
    }
  }
}