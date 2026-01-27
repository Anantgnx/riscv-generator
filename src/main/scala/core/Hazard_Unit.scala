package core

import chisel3._
import chisel3.util.log2Up

class Hazard_Unit(c: Config) extends Module {
  val io = IO(new Bundle {
    val IF_ID_rs1 = Input(UInt(log2Up(c.numRegs).W))
    val IF_ID_rs2 = Input(UInt(log2Up(c.numRegs).W))
    val ID_EX_rd = Input(UInt(log2Up(c.numRegs).W))
    val ID_EX_MemRead = Input(Bool())
    val stall = Output(Bool())
  })

  when(io.ID_EX_MemRead && ((io.ID_EX_rd === io.IF_ID_rs1) || (io.ID_EX_rd === io.IF_ID_rs2))) {
    io.stall := true.B
  }
    .otherwise {
      io.stall := false.B
    }
}