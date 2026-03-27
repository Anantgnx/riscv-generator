package core

import chisel3._
import chisel3.util._

// Single-cycle instruction ROM. Purely combinational — no stall, no handshake.
// inst_out is valid the same cycle as pc_in.
class InstructionROM(c: Config) extends Module {
  val io = IO(new Bundle {
    val pc       = Input(UInt(c.xLen.W))
    val inst_out = Output(UInt(c.xLen.W))
  })

  // Program: edit this Vec to load your benchmark.
  // Word-addressed: word_addr = pc >> 2
  val rom = VecInit(Seq(
    "h00000513".U(32.W), // 0x00: addi x10, x0,  0     (sum = 0)
    "h00400613".U(32.W), // 0x04: addi x12, x0,  4     (counter = 4)
    "h00C50533".U(32.W), // 0x08: add  x10, x10, x12   <- LOOP TOP
    "hFFF60613".U(32.W), // 0x0c: addi x12, x12, -1
    "hFE061CE3".U(32.W), // 0x10: bne  x12, x0,  -8    <- back to 0x08
    "h00A02533".U(32.W), // 0x14: slt  x10, x0,  x10   <- SLT test
    "h00000000".U(32.W), // 0x18: nop (exit)
    "h00000000".U(32.W)  // 0x1c: nop
  ))

  val word_addr = io.pc >> 2
  io.inst_out  := Mux(word_addr < rom.length.U, rom(word_addr(log2Ceil(rom.length) - 1, 0)), 0.U)
}