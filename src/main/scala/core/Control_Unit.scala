package core

import chisel3._
import chisel3.util._
import chisel3.util.log2Up

class Control_Unit(c: Config) extends Module {
  val io = IO(new Bundle {
    val instruction = Input(UInt(c.xLen.W))
    val stall       = Input(Bool())

    val ALU_src     = Output(Bool())
    val Reg_write   = Output(Bool())
    val Mem_write   = Output(Bool())
    val MemRead     = Output(Bool())
    val ALU_op      = Output(UInt(4.W))
    val MemtoReg    = Output(Bool())
    val Branch      = Output(Bool())
  })

  val opcode = io.instruction(6, 0)
  val funct3 = io.instruction(14, 12)
  val bit30  = io.instruction(30)

  val R_TYPE    = "b0110011".U
  val LOAD      = "b0000011".U
  val STORE     = "b0100011".U
  val I_TYPE    = "b0010011".U
  val BRANCH    = "b1100011".U

  io.ALU_src   := false.B
  io.Reg_write := false.B
  io.Mem_write := false.B
  io.MemRead   := false.B
  io.MemtoReg  := false.B
  io.Branch    := false.B
  io.ALU_op    := 0.U

  when(!io.stall) {
    switch(opcode) {
      is(R_TYPE) {
        io.ALU_src   := false.B
        io.Reg_write := true.B
        io.ALU_op    := Cat(funct3, bit30)
      }
      is(LOAD) {
        io.ALU_src   := true.B
        io.Reg_write := true.B
        io.MemRead   := true.B
        io.MemtoReg  := true.B
        io.ALU_op    := "b0010".U
      }
      is(STORE) {
        io.ALU_src   := true.B
        io.Mem_write := true.B
        io.ALU_op    := "b0010".U
      }
      is(I_TYPE) {
        io.ALU_src   := true.B
        io.Reg_write := true.B
        io.ALU_op    := Cat(funct3, 0.U(1.W))
      }
      is(BRANCH) {
        io.Branch := true.B
        io.ALU_op := "b0001".U
      }
    }
  }
}