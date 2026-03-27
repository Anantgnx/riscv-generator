package core

import chisel3._
import chisel3.util._

class Cache(c: Config) extends Module {
  val io = IO(new Bundle {
    val cpu_addr       = Input(UInt(c.xLen.W))
    val cpu_write_data = Input(UInt(c.xLen.W))
    val cpu_read_en    = Input(Bool())
    val cpu_write_en   = Input(Bool())
    val cpu_read_data  = Output(UInt(c.xLen.W))
    val stall_cpu      = Output(Bool())
    val debug_state    = Output(UInt(3.W))
    val mem            = new MemPort(c)
  })

  // --- Parameters & Address Decoding ---
  val num_sets = ((c.cacheSizeKB * 1024) / (c.xLen / 8) / c.cacheAssociativity)
  val index_w  = log2Up(num_sets)
  val offset_w = log2Up(c.xLen / 8)
  val tag_w    = 32 - (offset_w + index_w)

  val addr_index = io.cpu_addr(index_w + offset_w - 1, offset_w)
  val addr_tag   = io.cpu_addr(31, offset_w + index_w)

  // --- Storage ---
  val tag_arrays      = Seq.fill(c.cacheAssociativity)(Mem(num_sets, UInt(tag_w.W)))
  val data_arrays     = Seq.fill(c.cacheAssociativity)(Mem(num_sets, UInt(c.xLen.W)))
  val valid_bit_array = RegInit(VecInit(Seq.fill(c.cacheAssociativity)(VecInit(Seq.fill(num_sets)(false.B)))))

  // --- State Machine ---
  val sIdle :: sLookup :: sCompare :: sRefill :: Nil = Enum(4)
  val state = RegInit(sIdle)

  val addr_reg        = Reg(UInt(index_w.W))
  val addr_tag_reg    = Reg(UInt(tag_w.W))
  val full_addr_reg   = Reg(UInt(32.W))
  val replacement_way = RegInit(0.U(log2Up(c.cacheAssociativity).W))

  // --- Combinational tag/data lookup ---
  val sram_index  = Mux(state === sIdle, addr_index, addr_reg)
  val current_tag = Mux(state === sIdle, addr_tag,   addr_tag_reg)

  val tags_out = Wire(Vec(c.cacheAssociativity, UInt(tag_w.W)))
  val data_out = Wire(Vec(c.cacheAssociativity, UInt(c.xLen.W)))
  val hits     = Wire(Vec(c.cacheAssociativity, Bool()))

  for (i <- 0 until c.cacheAssociativity) {
    tags_out(i) := tag_arrays(i).read(sram_index)
    data_out(i) := data_arrays(i).read(sram_index)
    hits(i)     := (tags_out(i) === current_tag) && valid_bit_array(i)(sram_index)
  }

  val hit  = hits.asUInt.orR
  val done = io.mem.mem_valid

  // --- State Transitions ---
  switch(state) {
    is(sIdle) {
      when(io.cpu_read_en || io.cpu_write_en) {
        state         := sLookup
        addr_reg      := addr_index
        addr_tag_reg  := addr_tag
        full_addr_reg := io.cpu_addr
      }
    }
    is(sLookup) {
      state := sCompare
    }
    is(sCompare) {
      when(hit) {
        if (!c.isReadOnlyCache) {
          when(io.cpu_write_en) {
            for (i <- 0 until c.cacheAssociativity) {
              when(hits(i)) { data_arrays(i).write(addr_reg, io.cpu_write_data) }
            }
          }
        }
        state := sIdle
      } .otherwise {
        state := Mux(io.cpu_write_en && !c.isReadOnlyCache.B, sIdle, sRefill)
      }
    }
    is(sRefill) {
      when(done) {
        for (i <- 0 until c.cacheAssociativity) {
          when(replacement_way === i.U) {
            tag_arrays(i).write(addr_reg, addr_tag_reg)
            data_arrays(i).write(addr_reg, io.mem.mem_read_data)
            valid_bit_array(i)(addr_reg) := true.B
          }
        }
        replacement_way := replacement_way + 1.U
        state           := sIdle
      }
    }
  }

  // --- Registered output ---
  // Written on the last active cycle. stall_cpu held one extra cycle
  // (was_busy) so the pipeline sees stable data when it unfreezes.
  val read_data_reg = RegInit(0.U(c.xLen.W))
  when(state === sCompare && hit) {
    read_data_reg := Mux1H(hits, data_out)
  } .elsewhen(state === sRefill && done) {
    read_data_reg := io.mem.mem_read_data
  }
  io.cpu_read_data := read_data_reg

  val was_busy  = RegNext(state =/= sIdle, false.B)
  io.stall_cpu := (state =/= sIdle) || was_busy

  // Memory port
  io.mem.mem_addr       := full_addr_reg
  io.mem.mem_read_en    := (state === sRefill)
  io.mem.mem_write_en   := (!c.isReadOnlyCache.B && state === sCompare && io.cpu_write_en && hit)
  io.mem.mem_write_data := io.cpu_write_data

  io.debug_state := state.asUInt
}