package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PipelineRegsTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pipeline Registers"
  val c = Config()

  it should "test IF/ID: handle stall and flush" in {
    // Note: ensure you added 'extends Module' to pipeline_reg_if_id
    test(new pipeline_reg_if_id(c)) { dut =>
      // Initial state
      dut.io.stall.poke(false.B)
      dut.io.flush.poke(false.B)
      dut.io.pc_in.poke(4.U)
      dut.io.instruction_in.poke("h00A00513".U)
      dut.clock.step(1)

      // Check normal update
      dut.io.pc_out.expect(4.U)
      dut.io.instruction_out.expect("h00A00513".U)

      // Test Stall
      dut.io.stall.poke(true.B)
      dut.io.pc_in.poke(8.U)
      dut.clock.step(1)
      dut.io.pc_out.expect(4.U) // Should remain at 4

      // Test Flush (Stall off)
      dut.io.stall.poke(false.B)
      dut.io.flush.poke(true.B)
      dut.clock.step(1)
      dut.io.pc_out.expect(0.U)
      dut.io.instruction_out.expect(0.U)
    }
  }

  it should "test ID/EX: pass control and data signals" in {
    test(new pipeline_reg_id_ex(c)) { dut =>
      dut.io.flush.poke(false.B)
      dut.io.pc_in.poke(12.U)
      dut.io.rs1_in.poke(1.U)
      dut.io.ALU_op_in.poke(2.U) // ADD
      dut.io.reg_write_in.poke(true.B)

      dut.clock.step(1)

      dut.io.pc_out.expect(12.U)
      dut.io.rs1_out.expect(1.U)
      dut.io.ALU_op_out.expect(2.U)
      dut.io.reg_write_out.expect(true.B)

      // Test Flush
      dut.io.flush.poke(true.B)
      dut.clock.step(1)
      dut.io.reg_write_out.expect(false.B)
      dut.io.pc_out.expect(0.U)
    }
  }

  it should "test EX/MEM: simple cycle delay" in {
    test(new pipeline_reg_ex_mem(c)) { dut =>
      dut.io.alu_result_in.poke("hABCD".U)
      dut.io.mem_write_in.poke(true.B)
      dut.io.rd_in.poke(10.U)

      dut.clock.step(1)

      dut.io.alu_result_out.expect("hABCD".U)
      dut.io.mem_write_out.expect(true.B)
      dut.io.rd_out.expect(10.U)
    }
  }

  it should "test MEM/WB: simple cycle delay" in {
    test(new pipeline_reg_mem_wb(c)) { dut =>
      dut.io.read_data_in.poke("h1234".U)
      dut.io.reg_write_in.poke(true.B)

      dut.clock.step(1)

      dut.io.read_data_out.expect("h1234".U)
      dut.io.reg_write_out.expect(true.B)
    }
  }
}