package core

import chisel3._
import chisel3.util._

class ALU(c: Config) extends Module {
  val io = IO(new Bundle {
    val op1         = Input(SInt(c.xLen.W))
    val op2         = Input(SInt(c.xLen.W))
    val alu_control = Input(UInt(4.W))
    val alu_result  = Output(SInt(c.xLen.W))
    val zero        = Output(Bool())
  })

  val shamt = io.op2(4, 0).asUInt

  io.alu_result := 0.S

  switch(io.alu_control) {
    is(0.U)  { io.alu_result := io.op1 + io.op2 }                                      // ADD/ADDI
    is(1.U)  { io.alu_result := io.op1 - io.op2 }                                      // SUB / branch compare
    is(2.U)  { io.alu_result := (io.op1.asUInt << shamt).asSInt }                      // SLL/SLLI
    is(3.U)  { if (c.hasMul) io.alu_result := (io.op1 * io.op2)(c.xLen-1, 0).asSInt }  // MUL
    is(4.U)  { io.alu_result := Mux(io.op1 < io.op2, 1.S, 0.S) }                       // SLT/SLTI
    is(6.U)  { io.alu_result := Mux(io.op1.asUInt < io.op2.asUInt, 1.S, 0.S) }         // SLTU/SLTIU
    is(7.U)  { io.alu_result := io.op1 + 4.S }                                         // JALR: rd=PC+4
    is(8.U)  { io.alu_result := (io.op1 ^ io.op2) }                                    // XOR/XORI
    is(9.U)  { io.alu_result := io.op1 + 4.S }                                         // JAL:  rd=PC+4
    is(10.U) { io.alu_result := (io.op1.asUInt >> shamt).asSInt }                      // SRL/SRLI
    is(11.U) { io.alu_result := (io.op1 >> shamt.asUInt) }                             // SRA/SRAI
    is(12.U) { io.alu_result := (io.op1 | io.op2) }                                    // OR/ORI
    is(13.U) { io.alu_result := io.op1 + io.op2 }                                      // AUIPC: PC + imm<<12 (op1=PC injected by Top)
    is(14.U) { io.alu_result := (io.op1 & io.op2) }                                    // AND/ANDI
    is(15.U) { io.alu_result := io.op2 }                                               // LUI: pass-through imm<<12
  }

  io.zero := io.alu_result === 0.S
}