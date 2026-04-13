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
  val bit25  = io.instruction(25)

  val R_TYPE = "b0110011".U
  val LOAD   = "b0000011".U
  val STORE  = "b0100011".U
  val I_TYPE = "b0010011".U
  val BRANCH = "b1100011".U
  val LUI    = "b0110111".U
  val AUIPC  = "b0010111".U
  val JAL    = "b1101111".U
  val JALR   = "b1100111".U

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
        io.ALU_op    := Mux(bit25, 3.U, Cat(funct3, bit30))
      }
      is(LOAD) {
        io.ALU_src   := true.B
        io.Reg_write := true.B
        io.MemRead   := true.B
        io.MemtoReg  := true.B
        io.ALU_op    := 0.U
      }
      is(STORE) {
        io.ALU_src   := true.B
        io.Mem_write := true.B
        io.ALU_op    := 0.U
      }
      is(I_TYPE) {
        io.ALU_src   := true.B
        io.Reg_write := true.B
        when(funct3 === "b101".U) {
          io.ALU_op := Cat(funct3, bit30)  // SRLI=10, SRAI=11
        }.otherwise {
          io.ALU_op := Cat(funct3, 0.U(1.W))
        }
      }
      is(BRANCH) {
        io.Branch := true.B
        io.ALU_op := MuxLookup(funct3, 1.U)(Seq(
          "b000".U -> 1.U,   // BEQ  → SUB
          "b001".U -> 1.U,   // BNE  → SUB
          "b100".U -> 4.U,   // BLT  → SLT
          "b101".U -> 4.U,   // BGE  → SLT
          "b110".U -> 6.U,   // BLTU → SLTU
          "b111".U -> 6.U    // BGEU → SLTU
        ))
      }
      is(LUI) {
        io.ALU_src   := true.B  // use immediate as op2
        io.Reg_write := true.B
        io.ALU_op    := 15.U    // pass-through op2 (= imm<<12 from ImmGen)
        // op1 = x0 = 0, so result = imm<<12
      }
      is(AUIPC) {
        io.ALU_src   := true.B  // use immediate as op2
        io.Reg_write := true.B
        io.ALU_op    := 13.U    // PC + op2 (handled specially in Top.scala)
      }
      is(JAL) {
        io.Reg_write := true.B
        io.ALU_op    := 9.U     // rd=PC+4 (op1=PC injected by Top, result=PC+4)
        // PC target = PC + J-imm handled in Top.scala
      }
      is(JALR) {
        io.ALU_src   := true.B  // use imm as op2 (for target computation in Top)
        io.Reg_write := true.B
        io.ALU_op    := 7.U     // rd=PC+4 (op1=PC injected by Top, result=PC+4)
        // PC target = (rs1 + imm) & ~1 handled in Top.scala
      }
    }
  }
}