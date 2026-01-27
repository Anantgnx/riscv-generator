package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ImmGenTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Immediate Generator"
  val c = Config()

  it should "generate correct immediates for I, S, and B types" in {
    test(new ImmGen(c)) { dut =>

      // ---------------------------------------------------------
      // 1. Test I-Type (ADDI)
      // ---------------------------------------------------------
      // Value: -2 (in 12-bit 2's complement is 111111111110 -> 0xFFE)
      // Construction: Imm(111111111110) | rs1(00000) | funct3(000) | rd(00000) | opcode(0010011)
      // Binary: 111111111110_00000_000_00000_0010011
      val iTypeInstr = "b111111111110_00000_000_00000_0010011".U

      dut.io.instr.poke(iTypeInstr)
      // Expected: 32-bit representation of -2 (FFFF FFFE)
      dut.io.imm.expect("hFFFFFFFE".U)


      // ---------------------------------------------------------
      // 2. Test S-Type (STORE)
      // ---------------------------------------------------------
      // Value: 5
      // Construction: imm[11:5](0000000) | rs2(00000) | rs1(00000) | funct3(010) | imm[4:0](00101) | opcode(0100011)
      val sTypeInstr = "b0000000_00000_00000_010_00101_0100011".U

      dut.io.instr.poke(sTypeInstr)
      dut.io.imm.expect(5.U)


      // ---------------------------------------------------------
      // 3. Test B-Type (BRANCH)
      // ---------------------------------------------------------
      // Value: 2 (Target is PC + 2)
      // Note: B-type immediate logic appends a 0 at bit 0.
      // So to get "2", bits [12:1] must represent "1".
      // bit 12=0, bit 11=0... bit 1=1.
      // Slicing: bit[12](0) | bit[10:5](000000) | bit[4:1](0001) | bit[11](0)

      // Breakdown of input instruction bits:
      // imm[12](0) | imm[10:5](000000) | rs2(0) | rs1(0) | f3(0) | imm[4:1](0001) | imm[11](0) | opcode(1100011)
      // Binary: 0_000000_00000_00000_000_0001_0_1100011
      val bTypeInstr = "b00000000000000000000000101100011".U

      dut.io.instr.poke(bTypeInstr)
      dut.io.imm.expect(2.U)


      // ---------------------------------------------------------
      // 4. Test Unknown Opcode (Default case)
      // ---------------------------------------------------------
      dut.io.instr.poke("hFFFFFFFF".U) // All ones (invalid opcode usually)
      dut.io.imm.expect(0.U)
    }
  }
}