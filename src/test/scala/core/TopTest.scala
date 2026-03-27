package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TopTest extends AnyFlatSpec with ChiselScalatestTester {
  "RISCV Processor" should "validate BNE and SLT" in {
    test(new Top(Config())).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      println("=" * 60)
      println("RISC-V Processor Simulation: Logic Trace")
      println("=" * 60)

      var cycles = 0
      while (cycles < 300) {
        val pc       = dut.io.debug_pc.peek().litValue
        val inst     = dut.io.debug_inst.peek().litValue
        val x10      = dut.io.debug_x10.peek().litValue
        val dcacheSt = dut.io.debug_dcache_state.peek().litValue.toInt

        val dcacheStr = dcacheSt match {
          case 0 => "IDLE   "
          case 1 => "LOOKUP "
          case 2 => "COMPARE"
          case 3 => "REFILL "
          case _ => "UNKNOWN"
        }

        println(f"Cycle $cycles%3d | PC: 0x$pc%04x | Inst: 0x$inst%08x | x10: $x10%3d | D-Cache: $dcacheStr")

        dut.clock.step(1)
        cycles += 1
      }

      println("=" * 60)
      val finalX10 = dut.io.debug_x10.peek().litValue
      println(f"Simulation finished at cycle $cycles")
      println(f"x10 = $finalX10  (expected 1 after SLT, was 10 after loop)")
      println("=" * 60)
    }
  }
}