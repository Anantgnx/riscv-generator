package core

import chisel3._
import chisel3.util._

// Data RAM — 512 words, initialized per benchmark using VecInit.
// VecInit is synthesized as a register file with compile-time-fixed reset values,
// which guarantees correct initial values unlike Mem.write in a reset block.

class DataRAM(c: Config) extends Module {
  val io        = IO(Flipped(new MemPort(c)))
  val debug_c00 = IO(Output(UInt(c.xLen.W)))  // C[0][0] at word 40 = 0xA0
  val debug_c01 = IO(Output(UInt(c.xLen.W)))  // C[0][1] at word 41 = 0xA4
  val debug_c10 = IO(Output(UInt(c.xLen.W)))  // C[1][0] at word 42 = 0xA8
  val debug_c11 = IO(Output(UInt(c.xLen.W)))  // C[1][1] at word 43 = 0xAC

  // --- Latency counter ---
  val mem_latency = c.memLatency.U
  val count       = RegInit(0.U(8.W))
  val is_busy     = io.mem_read_en || io.mem_write_en

  when(is_busy) {
    when(count === mem_latency) { count := 0.U }
      .otherwise                { count := count + 1.U }
  } .otherwise { count := 0.U }

  io.mem_valid := (count === mem_latency) && is_busy

  val word_addr = (io.mem_addr >> 2).asUInt(8, 0)  // 9-bit word index (512 words)

  // --- Initial data per benchmark ---
  def makeInit(): Vec[UInt] = {
    val init = scala.collection.mutable.ArrayBuffer.fill(512)(BigInt(0))
    c.benchmark match {
      case 0 =>
        // A=[[1,2],[3,4]] at words 32-35 (0x080-0x08C)
        // B=[[5,6],[7,8]] at words 36-39 (0x090-0x09C)
        // C=[[0,0],[0,0]] at words 40-43 (0x0A0-0x0AC)
        val A = Seq(1, 2, 3, 4)
        val B = Seq(5, 6, 7, 8)
        for (i <- 0 until 4) init(32 + i) = BigInt(A(i))
        for (i <- 0 until 4) init(36 + i) = BigInt(B(i))
      case 1 =>
        for (i <- 0 until 16) init(32 + i) = BigInt(17)
      case 2 =>
        val s = Seq(8, 7, 6, 5, 4, 3, 2, 1)
        for (i <- 0 until 8) init(32 + i) = BigInt(s(i))
      case 3 =>
        for (rep <- 0 until 4)
          for (v <- 0 until 8) init(32 + rep * 8 + v) = BigInt(v)
      case _ =>
    }
    VecInit(init.map(_.U(c.xLen.W)).toSeq)
  }

  val ram = RegInit(makeInit())

  // Read (combinational)
  io.mem_read_data := ram(word_addr)

  // Write (on valid cycle only)
  when(io.mem_write_en && io.mem_valid) {
    ram(word_addr) := io.mem_write_data
  }

  // Debug: always read C[1][1] = word 43 = address 0xAC
  debug_c00 := ram(40.U)
  debug_c01 := ram(41.U)
  debug_c10 := ram(42.U)
  debug_c11 := ram(43.U)
}