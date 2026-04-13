package core

import chisel3._
import chisel3.util.log2Up

class RegisterFile(c: Config) extends Module {
  val io = IO(new Bundle {
    val regWrite  = Input(Bool())
    val ra1       = Input(UInt(log2Up(c.numRegs).W))
    val ra2       = Input(UInt(log2Up(c.numRegs).W))
    val wa        = Input(UInt(log2Up(c.numRegs).W))
    val wd        = Input(SInt(c.xLen.W))
    val rd1       = Output(SInt(c.xLen.W))
    val rd2       = Output(SInt(c.xLen.W))
    val debug_x1  = Output(UInt(c.xLen.W))
    val debug_x10 = Output(UInt(c.xLen.W))
    val debug_x11 = Output(UInt(c.xLen.W))
    val debug_x12 = Output(UInt(c.xLen.W))
    val debug_x13 = Output(UInt(c.xLen.W))
    val debug_x14 = Output(UInt(c.xLen.W))
    val debug_x15 = Output(UInt(c.xLen.W))
    val debug_x16 = Output(UInt(c.xLen.W))
    val debug_x17 = Output(UInt(c.xLen.W))
    val debug_x18 = Output(UInt(c.xLen.W))
    val debug_x19 = Output(UInt(c.xLen.W))
    val debug_x20 = Output(UInt(c.xLen.W))
    val debug_x21 = Output(UInt(c.xLen.W))
    val debug_x22 = Output(UInt(c.xLen.W))
    val debug_x23 = Output(UInt(c.xLen.W))
    val debug_x24 = Output(UInt(c.xLen.W))
  })

  val registers = RegInit(VecInit(Seq.fill(c.numRegs)(0.S(c.xLen.W))))

  // Write-first: forward WB→ID same cycle
  val fwd1 = io.regWrite && io.wa =/= 0.U && io.wa === io.ra1
  val fwd2 = io.regWrite && io.wa =/= 0.U && io.wa === io.ra2
  io.rd1 := Mux(io.ra1 === 0.U, 0.S(c.xLen.W), Mux(fwd1, io.wd, registers(io.ra1)))
  io.rd2 := Mux(io.ra2 === 0.U, 0.S(c.xLen.W), Mux(fwd2, io.wd, registers(io.ra2)))

  when(io.regWrite && io.wa =/= 0.U) {
    registers(io.wa) := io.wd
  }

  io.debug_x1  := registers(1).asUInt
  io.debug_x10 := registers(10).asUInt
  io.debug_x11 := registers(11).asUInt
  io.debug_x12 := registers(12).asUInt
  io.debug_x13 := registers(13).asUInt
  io.debug_x14 := registers(14).asUInt
  io.debug_x15 := registers(15).asUInt
  io.debug_x16 := registers(16).asUInt
  io.debug_x17 := registers(17).asUInt
  io.debug_x18 := registers(18).asUInt
  io.debug_x19 := registers(19).asUInt
  io.debug_x20 := registers(20).asUInt
  io.debug_x21 := registers(21).asUInt
  io.debug_x22 := registers(22).asUInt
  io.debug_x23 := registers(23).asUInt
  io.debug_x24 := registers(24).asUInt
}