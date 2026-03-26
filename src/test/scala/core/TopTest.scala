package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TopTest extends AnyFlatSpec with ChiselScalatestTester {
  "RISCV Processor" should "execute Matrix Multiply and debug branch logic" in {
    test(new Top(Config())).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      println("=" * 110)
      println(f"${"Cycle"}%-6s | ${"PC"}%-6s | ${"Inst"}%-10s | ${"x10"}%-5s | ${"ALU"}%-5s | ${"Br?"}%-5s | ${"Target"}%-7s | ${"Stall"}%-8s")
      println("-" * 110)

      var cycles = 0
      while (cycles < 1000 && !dut.io.exit.peek().litToBoolean) {
        val pc        = dut.io.debug_pc.peek().litValue
        val inst      = dut.io.debug_inst.peek().litValue
        val x10       = dut.io.debug_x10.peek().litValue
        val brTaken   = dut.io.debug_branch_taken.peek().litToBoolean
        val brTarget  = dut.io.debug_branch_target.peek().litValue
        val aluRes    = dut.io.debug_alu_result.peek().litValue

        val stallStr = dut.io.debug_stall_src.peek().litValue.toInt match {
          case 1 => "ICache"
          case 2 => "DCache"
          case 3 => "Hazard"
          case 4 => "Start"
          case _ => "None"
        }

        println(f"$cycles%-6d | 0x$pc%04x | 0x$inst%08x | $x10%-5d | $aluRes%-5d | $brTaken%-5b | 0x$brTarget%04x | $stallStr%-8s")

        dut.clock.step(1)
        cycles += 1
      }

      println("=" * 110)
      println(f"Simulation Finished at Cycle $cycles")
      println(f"Final Value in x10: ${dut.io.debug_x10.peek().litValue}")
      println("=" * 110)
    }
  }
}