package core

import chisel3._
import chisel3.util._

class Arbiter(c: Config) extends Module {
  val io = IO(new Bundle {
    val icache     = Flipped(new MemPort(c))
    val dcache     = Flipped(new MemPort(c))
    val master_mem = new MemPort(c)
    val debug_state = Output(UInt(2.W))
  })

  val sIdle :: sICacheGranted :: sDCacheGranted :: Nil = Enum(3)
  val state = RegInit(sIdle)

  io.master_mem.mem_addr       := 0.U
  io.master_mem.mem_read_en    := false.B
  io.master_mem.mem_write_en   := false.B
  io.master_mem.mem_write_data := 0.U
  io.icache.mem_valid          := false.B
  io.dcache.mem_valid          := false.B
  io.icache.mem_read_data      := 0.U
  io.dcache.mem_read_data      := 0.U

  switch(state) {
    is(sIdle) {
      when(io.icache.mem_read_en) {
        state := sICacheGranted
      } .elsewhen(io.dcache.mem_read_en || io.dcache.mem_write_en) {
        state := sDCacheGranted
      }
    }
    is(sICacheGranted) {
      io.master_mem.mem_addr    := io.icache.mem_addr
      io.master_mem.mem_read_en := true.B
      io.icache.mem_valid       := io.master_mem.mem_valid
      io.icache.mem_read_data   := io.master_mem.mem_read_data
      when(io.master_mem.mem_valid) { state := sIdle }
    }
    is(sDCacheGranted) {
      io.master_mem.mem_addr       := io.dcache.mem_addr
      io.master_mem.mem_read_en    := io.dcache.mem_read_en
      io.master_mem.mem_write_en   := io.dcache.mem_write_en
      io.master_mem.mem_write_data := io.dcache.mem_write_data
      io.dcache.mem_valid          := io.master_mem.mem_valid
      io.dcache.mem_read_data      := io.master_mem.mem_read_data
      when(io.master_mem.mem_valid) { state := sIdle }
    }
  }

  io.debug_state := state.asUInt
}