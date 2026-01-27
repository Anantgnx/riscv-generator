package core

import chisel3._
import chisel3.util.log2Up

class RegisterFile(c: Config) extends Module {
  val io = IO(new Bundle{
    val regWrite = Input(Bool())
    val ra1 = Input(UInt(log2Up(c.numRegs).W))
    val ra2 = Input(UInt(log2Up(c.numRegs).W))
    val wa = Input(UInt(log2Up(c.numRegs).W))
    val wd = Input(SInt(c.xLen.W))
    val rd1 = Output(SInt(c.xLen.W))
    val rd2 = Output(SInt(c.xLen.W))
  })

  val registers = RegInit(VecInit(Seq.fill(c.numRegs)(0.S(c.xLen.W))))

  io.rd1 := Mux(io.ra1 === 0.U, 0.S(c.xLen.W), registers(io.ra1))
  io.rd2 := Mux(io.ra2 === 0.U, 0.S(c.xLen.W), registers(io.ra2))

  when(io.regWrite && io.wa =/= 0.U) {
    registers(io.wa) := io.wd
  }
}