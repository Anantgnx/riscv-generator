package core

import chisel3._
import chisel3.util._

class InstructionMemory extends Module {
  val io = IO(new Bundle {
    val address = Input(UInt(32.W))
    val data_out = Output(UInt(32.W))
  })

  // 1. Define the Program
  val program = Seq(
    "h00A00513".U(32.W), // 0: addi x10, x0, 10
    "h01400593".U(32.W), // 4: addi x11, x0, 20
    "h00B50633".U(32.W)  // 8: add  x12, x10, x11
  )

  // 2. Define the Padding (Zeros)
  // Calculate exactly how many zeros we need to fill the 1024 slots
  val padding = Seq.fill(1024 - program.length)(0.U(32.W))

  // 3. Create Memory (Program + Padding)
  val mem = VecInit(program ++ padding)

  // 4. Fetch Logic
  io.data_out := mem(io.address >> 2)
}