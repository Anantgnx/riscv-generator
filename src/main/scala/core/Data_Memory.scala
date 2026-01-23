package core

import chisel3._

class Data_Memory extends Module {
  val io = IO(new Bundle {
    val Daddress   = Input(UInt(32.W))
    val write_data = Input(UInt(32.W))
    val MemRead    = Input(Bool())
    val MemWrite   = Input(Bool())
    val read_data  = Output(UInt(32.W))
  })

  val memory = Mem(1024, UInt(32.W))

  when(io.MemWrite) {
    memory(io.Daddress(31, 2)) := io.write_data
  }

  io.read_data := memory(io.Daddress(31, 2))
}