package core

import chisel3._

class RegisterFile extends Module {
  val io = IO(new Bundle{
    val regWrite = Input(Bool())
    val ra1 = Input(UInt(5.W))
    val ra2 = Input(UInt(5.W))
    val wa = Input(UInt(5.W))
    val wd = Input(SInt(32.W))
    val rd1 = Output(SInt(32.W))
    val rd2 = Output(SInt(32.W))
  })

  val registers = RegInit(VecInit(Seq.fill(32)(0.S(32.W))))

  io.rd1 := Mux(io.ra1 === 0.U, 0.S, registers(io.ra1))
  io.rd2 := Mux(io.ra2 === 0.U, 0.S, registers(io.ra2))

  when(io.regWrite && io.wa =/= 0.U) {
    registers(io.wa) := io.wd
  }
}