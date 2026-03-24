package core

import chisel3._
import chisel3.util._

class Arbiter(c: Config) extends Module {
  val io = IO(new Bundle {
    // Interface to Instruction Cache
    val icache = Flipped(new MemPort(c))
    // Interface to Data Cache
    val dcache = Flipped(new MemPort(c))
    // Interface to Main Memory
    val master_mem = new MemPort(c)
  })

  io.icache.mem_read_data := io.master_mem.mem_read_data
  io.dcache.mem_read_data := io.master_mem.mem_read_data

  // 1. Default assignments
  io.master_mem.mem_addr     := 0.U
  io.master_mem.mem_read_en  := false.B
  io.master_mem.mem_write_en := false.B
  io.master_mem.mem_write_data := 0.U
  io.icache.mem_valid        := false.B
  io.dcache.mem_valid        := false.B

  when(io.icache.mem_read_en) {
    // I-Cache gets the bus
    io.master_mem.mem_addr     := io.icache.mem_addr
    io.master_mem.mem_read_en  := true.B

    // Only send the 'valid' signal back to the I-Cache
    io.icache.mem_valid        := io.master_mem.mem_valid
  } .elsewhen(io.dcache.mem_read_en || io.dcache.mem_write_en) {
    // D-Cache gets the bus only if I-Cache is quiet
    io.master_mem.mem_addr       := io.dcache.mem_addr
    io.master_mem.mem_read_en    := io.dcache.mem_read_en
    io.master_mem.mem_write_en   := io.dcache.mem_write_en
    io.master_mem.mem_write_data := io.dcache.mem_write_data

    // Only send the 'valid' signal back to the D-Cache
    io.dcache.mem_valid          := io.master_mem.mem_valid
  }
}