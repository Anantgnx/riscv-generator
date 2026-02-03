package core

import chisel3._
import chisel3.util._
import chisel3.util.log2Up

class ALU(c: Config) extends Module {
  val io = IO(new Bundle {
      val op1 = Input(SInt(c.xLen.W)) // First operand
      val op2 = Input(SInt(c.xLen.W)) // Second operand
      val alu_control = Input(UInt(4.W)) // Alu opcode
      val alu_result = Output(SInt(c.xLen.W)) // AlU output
      val zero = Output(Bool()) // Zero flag
  })

  io.alu_result := 0.S

  switch(io.alu_control) { // switch statement
    is(0.U) { io.alu_result := io.op1 + io.op2 }
    is(2.U) { io.alu_result := io.op1 + io.op2 }
    is(1.U) { io.alu_result := io.op1 - io.op2 }
    is(3.U) { if (c.hasMul) {io.alu_result := (io.op1 * io.op2)(c.xLen - 1, 0).asSInt } }
    is(4.U) { io.alu_result := Mux(io.op1 < io.op2, 1.S, 0.S) }
    is(12.U) { io.alu_result := io.op1 | io.op2 }
    is(14.U) { io.alu_result := io.op1 & io.op2 }
  }

  io.zero := io.alu_result === 0.S
}