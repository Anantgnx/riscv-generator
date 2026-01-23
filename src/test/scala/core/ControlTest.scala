package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ControlTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Control Unit"

  it should "decode R-Type instructions correctly" in {
    // UPDATED: Use 'Control_Unit' instead of 'Control'
    test(new Control_Unit) { dut =>
      // R-Type Opcode: 0110011
      // We need a full 32-bit instruction.
      // Example: add x1, x2, x3 -> funct7=0000000, rs2=00011, rs1=00010, funct3=000, rd=00001, opcode=0110011
      // Constructing a dummy R-type instruction with opcode 0110011 and funct3=000
      val rTypeInstr = "b0000000_00011_00010_000_00001_0110011".U

      dut.io.instruction.poke(rTypeInstr)
      dut.io.stall.poke(false.B) // IMPORTANT: Must set stall to false!

      // Check outputs (Note: Case sensitive matches your Control_Unit.scala)
      dut.io.Reg_write.expect(true.B)
      dut.io.ALU_src.expect(false.B)
      dut.io.MemRead.expect(false.B)

      // ALU Op check: funct3(000) & bit30(0) -> 0000
      dut.io.ALU_op.expect("b0000".U)
    }
  }

  it should "decode Load instructions correctly" in {
    test(new Control_Unit) { dut =>
      // Load Opcode: 0000011 (lw)
      val loadInstr = "b000000000000_00010_010_00001_0000011".U

      dut.io.instruction.poke(loadInstr)
      dut.io.stall.poke(false.B)

      dut.io.Reg_write.expect(true.B)
      dut.io.ALU_src.expect(true.B)
      dut.io.MemRead.expect(true.B)
      dut.io.MemtoReg.expect(true.B)
      dut.io.ALU_op.expect("b0010".U) // Force ADD
    }
  }

  it should "handle Stall correctly" in {
    test(new Control_Unit) { dut =>
      // Feed a valid instruction but assert STALL
      val loadInstr = "b000000000000_00010_010_00001_0000011".U

      dut.io.instruction.poke(loadInstr)
      dut.io.stall.poke(true.B) // <--- STALL IS ON

      // Everything should be 0/false
      dut.io.Reg_write.expect(false.B)
      dut.io.MemRead.expect(false.B)
    }
  }
}