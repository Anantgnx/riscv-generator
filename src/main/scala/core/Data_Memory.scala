package core

import chisel3._
import chisel3.util._

// Data RAM with configurable latency.
// Connected to dcache via MemPort (same handshake as old MainMemory).
// Supports both reads and writes.
class DataRAM(c: Config) extends Module {
  val io = IO(Flipped(new MemPort(c)))

  val mem_latency = c.memLatency.U

  val count   = RegInit(0.U(8.W))
  val is_busy = io.mem_read_en || io.mem_write_en

  when(is_busy) {
    when(count === mem_latency) { count := 0.U }
      .otherwise                { count := count + 1.U }
  } .otherwise { count := 0.U }

  io.mem_valid := (count === mem_latency) && is_busy

  val word_addr = (io.mem_addr >> 2).asUInt

  // Data storage — initialise with your benchmark data arrays here.
  // Size: 512 words = 2KB. Adjust as needed.
  val ram = Mem(512, UInt(c.xLen.W))

  // Read
  io.mem_read_data := ram(word_addr(8, 0))

  // Write
  when(io.mem_write_en && io.mem_valid) {
    ram(word_addr(8, 0)) := io.mem_write_data
  }
}