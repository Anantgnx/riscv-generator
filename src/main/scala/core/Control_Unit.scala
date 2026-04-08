package core

import chisel3._
import chisel3.util._

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
    val funct3      = Output(UInt(3.W))
  })

  val opcode = io.instruction(6, 0)
  val funct3 = io.instruction(14, 12)
  val bit30  = io.instruction(30)
  val bit25  = io.instruction(25)  // M-extension flag

  val R_TYPE = "b0110011".U
  val LOAD   = "b0000011".U
  val STORE  = "b0100011".U
  val I_TYPE = "b0010011".U
  val BRANCH = "b1100011".U

  io.ALU_src   := false.B
  io.Reg_write := false.B
  io.Mem_write := false.B
  io.MemRead   := false.B
  io.MemtoReg  := false.B
  io.Branch    := false.B
  io.ALU_op    := 0.U
  io.funct3    := funct3

  when(!io.stall) {
    switch(opcode) {
      is(R_TYPE) {
        io.Reg_write := true.B
        // M-ext MUL: bit25=1 → ALU_op=3
        // Standard R-type: Cat(funct3, bit30)
        io.ALU_op := Mux(bit25, 3.U, Cat(funct3, bit30))
      }
      is(LOAD) {
        io.ALU_src   := true.B
        io.Reg_write := true.B
        io.MemRead   := true.B
        io.MemtoReg  := true.B
        io.ALU_op    := 0.U  // ADD: base + offset
      }
      is(STORE) {
        io.ALU_src   := true.B
        io.Mem_write := true.B
        io.ALU_op    := 0.U  // ADD: base + offset
      }
      is(I_TYPE) {
        io.ALU_src   := true.B
        io.Reg_write := true.B
        // For shifts (funct3=001 SLL, funct3=101 SRL/SRA):
        //   bit30=1 means SRAI → ALU_op=11, else SRLI → ALU_op=10
        //   SLLI always bit30=0 → ALU_op=2
        // For all other I-types: Cat(funct3, 0) matches R-type encoding
        when(funct3 === "b101".U) {
          // SRLI (bit30=0) → 10, SRAI (bit30=1) → 11
          io.ALU_op := Cat(funct3, bit30)
        }.otherwise {
          io.ALU_op := Cat(funct3, 0.U(1.W))
        }
      }
      is(BRANCH) {
        io.Branch := true.B
        io.ALU_op := 1.U  // SUB for BEQ/BNE comparison
      }
    }
  }
}