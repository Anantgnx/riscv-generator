package core

import chisel3._
import chisel3.util._
import chisel3.util.log2Up

class ImmGen(c: Config) extends Module {
  val io = IO(new Bundle {
    val instr = Input(UInt(c.xLen.W))
    val imm   = Output(UInt(c.xLen.W)) // Keeping as UInt to match your Verilog [31:0]
  })

  val opcode = io.instr(6, 0)

  // Default value
  io.imm := 0.U

  switch(opcode) {
    // I-type (Arithmetic) - 0010011
    is("b0010011".U) {
      // Verilog: {{20{instr[31]}}, instr[31:20]}
      io.imm := Cat(Fill(20, io.instr(31)), io.instr(31, 20))
    }

    // I-type (Load) - 0000011
    is("b0000011".U) {
      // Verilog: {{20{instr[31]}}, instr[31:20]}
      io.imm := Cat(Fill(20, io.instr(31)), io.instr(31, 20))
    }

    // S-type (Store) - 0100011
    is("b0100011".U) {
      // Verilog: {{20{instr[31]}}, instr[31:25], instr[11:7]}
      io.imm := Cat(Fill(20, io.instr(31)), io.instr(31, 25), io.instr(11, 7))
    }

    // B-type (Branch) - 1100011
    is("b1100011".U) {
      // Verilog: {{20{instr[31]}}, instr[7], instr[30:25], instr[11:8], 1'b0}
      io.imm := Cat(Fill(20, io.instr(31)), io.instr(7), io.instr(30, 25), io.instr(11, 8), 0.U(1.W))
    }
  }
}