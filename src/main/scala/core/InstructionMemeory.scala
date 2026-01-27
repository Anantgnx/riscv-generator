package core

import chisel3._
import chisel3.util._
import chisel3.util.log2Up

class InstructionMemory(c: Config) extends Module {
  val io = IO(new Bundle {
    val address = Input(UInt(c.xLen.W))
    val data_out = Output(UInt(c.xLen.W))
  })

  // 1. Define the Program
  val program = Seq(
    "h00A00513".U(c.xLen.W), // 0: addi x10, x0, 10
    "h01400593".U(c.xLen.W), // 4: addi x11, x0, 20
    "h00B50633".U(c.xLen.W)  // 8: add  x12, x10, x11
  )

  // 2. Define the Padding (Zeros)
  // Calculate exactly how many zeros we need to fill the 1024 slots
  val padding = Seq.fill(1024 - program.length)(0.U(c.xLen.W))

  // 3. Create Memory (Program + Padding)
  val mem = VecInit(program ++ padding)

  // 4. Fetch Logic
  io.data_out := mem(io.address >> 2)
}