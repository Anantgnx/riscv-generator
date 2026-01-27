package core

import chisel3._
import chisel3.util.log2Up

class PC(c: Config) extends Module {
  val io = IO(new Bundle {
    val pc_in = Input(UInt(c.xLen.W))
    val stall = Input(Bool())
    val pc_out = Output(UInt(c.xLen.W))
  })

  val pcReg = RegInit(0.U(c.xLen.W))

  when(!io.stall) {
    pcReg := io.pc_in
  }

  io.pc_out := pcReg
}