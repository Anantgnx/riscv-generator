package core

import chisel3._

class PC extends Module {
  val io = IO(new Bundle {
    val pc_in = Input(UInt(32.W))
    val stall = Input(Bool())
    val pc_out = Output(UInt(32.W))
  })

  val pcReg = RegInit(0.U(32.W))

  when(!io.stall) {
    pcReg := io.pc_in
  }

  io.pc_out := pcReg
}