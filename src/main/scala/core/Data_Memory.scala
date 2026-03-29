package core

import chisel3._
import chisel3.util._

// Data RAM — 512 words, initialized per benchmark using VecInit.
// VecInit is synthesized as a register file with compile-time-fixed reset values,
// which guarantees correct initial values unlike Mem.write in a reset block.

class DataRAM(c: Config) extends Module {
  val io = IO(Flipped(new MemPort(c)))

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
  // 512 words total. Unspecified words default to 0.
  def makeInit(): Vec[UInt] = {
    val init = scala.collection.mutable.ArrayBuffer.fill(512)(BigInt(0))
    c.benchmark match {
      case 0 =>
        // 2x2 Matrix Multiply: C = A * B
        // A=[[1,2],[3,4]] at words 32-35 (0x080-0x08C)
        // B=[[5,6],[7,8]] at words 36-39 (0x090-0x09C)
        // C=[[0,0],[0,0]] at words 40-43 (0x0A0-0x0AC) zero-initialized
        val A = Seq(1, 2, 3, 4)
        val B = Seq(5, 6, 7, 8)
        for (i <- 0 until 4) init(32 + i) = BigInt(A(i))
        for (i <- 0 until 4) init(36 + i) = BigInt(B(i))
      // C stays zero (written by SW during execution)
      // Expected: C=[[19,22],[43,50]], x10=C[1][1]=50
      case 1 =>
        // Vector reduction: C[i] = 17 for i=0..15, expected sum = 272
        // 16 words at 0x080 (word 32)
        for (i <- 0 until 16) init(32 + i) = BigInt(17)
      case 2 =>
        // Sort array at word 32 (0x080): [8, 7, 6, 5, 4, 3, 2, 1]
        val s = Seq(8, 7, 6, 5, 4, 3, 2, 1)
        for (i <- 0 until 8) init(32 + i) = BigInt(s(i))
      case 3 =>
        // Input array at word 32 (0x080): 4 repetitions of [0..7] (32 inputs)
        for (rep <- 0 until 4)
          for (v <- 0 until 8) init(32 + rep * 8 + v) = BigInt(v)
      // Bins at word 64 (0x100): all 0 (already zero)
      case _ => // no data needed
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
}