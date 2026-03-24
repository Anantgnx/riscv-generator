package core

import chisel3._
import chisel3.util._

class MainMemory(c: Config) extends Module {
  val io = IO(Flipped(new MemPort(c)))

  // 1. Program Data (Hardcoded for the test)
  val program = VecInit(Seq(
    "h00000513".U(32.W), // 0x00: li x10, 0        (Accumulator)
    "h00400613".U(32.W), // 0x04: li x12, 4        (Loop Counter n=4)
    "h02000693".U(32.W), // 0x08: li x13, 32       (Base Addr A)
    "h04000713".U(32.W), // 0x0c: li x14, 64       (Base Addr B)

    // --- Loop Start (PC: 0x10) ---
    "h0006a783".U(32.W), // 0x10: lw x15, 0(x13)   (Load A[i])
    "h00072803".U(32.W), // 0x14: lw x16, 0(x14)   (Load B[i])
    "h010787b3".U(32.W), // 0x18: add x15, x15, x16 (Dot Product Step)
    "h00f50533".U(32.W), // 0x1c: add x10, x10, x15 (Accumulate Result)
    "h00468693".U(32.W), // 0x20: addi x13, x13, 4  (Increment A Ptr)
    "h00470713".U(32.W), // 0x24: addi x14, x14, 4  (Increment B Ptr)
    "hfff60613".U(32.W), // 0x28: addi x12, x12, -1 (n--)

    // --- The Loop Control ---
    // If x12 == 0, we jump OVER the backward branch to exit
    "h00060463".U(32.W), // 0x2c: beq x12, x0, 8    (If n==0, jump to 0x34)

    // --- The Unconditional Backward Branch ---
    // beq x0, x0, -32. Always jumps back to 0x10.
    "hfe0000e3".U(32.W), // 0x30: beq x0, x0, -32

    "h00000000".U(32.W)  // 0x34: nop (Exit point)
  ))

  // 2. Latency Counter Logic
  val count = RegInit(0.U(8.W)) // Fixed the "aa" typo here
  val mem_latency = 10.U
  val is_busy = io.mem_read_en || io.mem_write_en

  // We only increment the counter when the memory is being asked to do something
  when(is_busy) {
    when(count === mem_latency) {
      count := 0.U
    } .otherwise {
      count := count + 1.U
    }
  } .otherwise {
    count := 0.U
  }

  // 3. Handshake Signals
  // The memory is "valid" only on the exact cycle the counter hits the target latency
  io.mem_valid := (count === mem_latency) && is_busy

  // 4. Read/Write Logic
  val word_addr = io.mem_addr >> 2

  // Return program data if within bounds, otherwise return 0
  // Note: Using a Wire here ensures the data is available when io.mem_valid is high
  io.mem_read_data := Mux(word_addr < program.length.U, program(word_addr), 0.U)

  // val ram = Mem(1024, UInt(c.xLen.W))
  // when(io.mem_write_en) { ram.write(word_addr, io.mem_write_data) }
}