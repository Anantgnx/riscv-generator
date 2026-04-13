package core

import chisel3._
import chisel3.util._

class ImmGen(c: Config) extends Module {
  val io = IO(new Bundle {
    val instr = Input(UInt(c.xLen.W))
    val imm   = Output(UInt(c.xLen.W))
  })

  val opcode = io.instr(6, 0)

  val I_TYPE = "b0010011".U  // ADDI, XORI, ORI, ANDI, SLLI, SRLI, SRAI, SLTI, SLTIU
  val LOAD   = "b0000011".U  // LW, LH, LB etc
  val STORE  = "b0100011".U  // SW, SH, SB
  val BRANCH = "b1100011".U  // BEQ, BNE, BLT, BGE, BLTU, BGEU
  val LUI    = "b0110111".U  // LUI
  val AUIPC  = "b0010111".U  // AUIPC
  val JAL    = "b1101111".U  // JAL  (Tier 3, placeholder)
  val JALR   = "b1100111".U  // JALR (Tier 3, placeholder)

  // I-type: inst[31:20] sign-extended
  val imm_i = Cat(Fill(20, io.instr(31)), io.instr(31, 20))

  // S-type: {inst[31:25], inst[11:7]} sign-extended
  val imm_s = Cat(Fill(20, io.instr(31)), io.instr(31, 25), io.instr(11, 7))

  // B-type: {inst[31], inst[7], inst[30:25], inst[11:8], 0} sign-extended
  val imm_b = Cat(Fill(19, io.instr(31)), io.instr(31), io.instr(7),
    io.instr(30, 25), io.instr(11, 8), 0.U(1.W))

  // U-type: inst[31:12] << 12 (lower 12 bits = 0)
  val imm_u = Cat(io.instr(31, 12), 0.U(12.W))

  // J-type (JAL): {inst[31], inst[19:12], inst[20], inst[30:21], 0} sign-extended
  val imm_j = Cat(Fill(11, io.instr(31)), io.instr(31), io.instr(19, 12),
    io.instr(20), io.instr(30, 21), 0.U(1.W))

  io.imm := MuxLookup(opcode, imm_i)(Seq(
    I_TYPE -> imm_i,
    LOAD   -> imm_i,
    JALR   -> imm_i,
    STORE  -> imm_s,
    BRANCH -> imm_b,
    LUI    -> imm_u,
    AUIPC  -> imm_u,
    JAL    -> imm_j
  ))
}