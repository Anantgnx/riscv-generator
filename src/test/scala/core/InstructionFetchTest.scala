package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class InstructionFetchTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Instruction Fetch Stage"

  it should "fetch instructions sequentially using Verilator" in {
    // FIX: Add 'VerilatorBackendAnnotation' here
    test(new Module {
      val io = IO(new Bundle {
        val instr = Output(UInt(32.W))
        val pc    = Output(UInt(32.W))
      })

      val pc  = Module(new PC())
      val mem = Module(new InstructionMemory())

      mem.io.address := pc.io.pc_out

      pc.io.pc_in := pc.io.pc_out + 4.U
      pc.io.stall := false.B

      io.pc    := pc.io.pc_out
      io.instr := mem.io.data_out

    }).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>  // <--- THE FIX

      // Cycle 0
      dut.io.pc.expect(0.U)
      dut.io.instr.expect("h00A00513".U)
      dut.clock.step(1)

      // Cycle 1
      dut.io.pc.expect(4.U)
      dut.io.instr.expect("h01400593".U)
      dut.clock.step(1)

      // Cycle 2
      dut.io.pc.expect(8.U)
      dut.io.instr.expect("h00B50633".U)
    }
  }
}