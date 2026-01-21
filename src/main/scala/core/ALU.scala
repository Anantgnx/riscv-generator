package core

import chisel3._
import chisel3.util._

class ALU extends Module {
  val io = IO(new Bundle {
      val op1 = Input(SInt(32.W)) // First operand
      val op2 = Input(SInt(32.W)) // Second operand
      val alu_control = Input(UInt(4.W)) // Alu opcode
      val alu_result = Output(SInt(32.W)) // AlU output
      val zero = Output(Bool()) // Zero flag
  })

  io.alu_result := 0.S

  switch(io.alu_control) { // switch statement
    is(0.U) { io.alu_result := io.op1 + io.op2 }
    is(2.U) { io.alu_result := io.op1 + io.op2 }
    is(1.U) { io.alu_result := io.op1 - io.op2 }
    is(4.U) { io.alu_result := Mux(io.op1 < io.op2, 1.S, 0.S) }
    is(12.U) { io.alu_result := io.op1 | io.op2 }
    is(14.U) { io.alu_result := io.op1 & io.op2 }
  }

  io.zero := io.alu_result === 0.S
}