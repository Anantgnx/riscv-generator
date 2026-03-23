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
    val mem_valid   = Output(Bool())
  })

  val memory = Mem(1024, UInt(c.xLen.W))

  val count = RegInit(0.U(4.W))
  val mem_latency = 10.U

  when(io.MemRead) {
    count := count + 1.U
  } .otherwise {
    count := 0.U
  }

  io.mem_valid := (count === mem_latency)

  when(io.MemWrite) {
    memory(io.Daddress(31, 2)) := io.write_data
  }

  io.read_data := memory(io.Daddress(31, 2))
}