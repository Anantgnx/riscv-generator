package core

import chisel3._
import chisel3.util.log2Up

class Data_Memory(c: Config) extends Module {
  val io = IO(new Bundle {
    val Daddress   = Input(UInt(c.xLen.W))
    val write_data = Input(UInt(c.xLen.W))
    val MemRead    = Input(Bool())
    val MemWrite   = Input(Bool())
    val read_data  = Output(UInt(c.xLen.W))
  })

  val memory = Mem(1024, UInt(c.xLen.W))

  when(io.MemWrite) {
    memory(io.Daddress(31, 2)) := io.write_data
  }

  io.read_data := memory(io.Daddress(31, 2))
}