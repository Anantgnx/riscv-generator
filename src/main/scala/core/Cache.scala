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
    val debug_hits     = Output(UInt(32.W))
    val debug_misses   = Output(UInt(32.W))
    val debug_rdr      = Output(UInt(32.W))
    val mem            = new MemPort(c)
  })

  // --- Parameters & Address Decoding ---
  val num_sets = ((c.cacheSizeKB * 1024) / (c.xLen / 8) / c.cacheAssociativity)
  val index_w  = log2Up(num_sets)
  val offset_w = log2Up(c.xLen / 8)
  val tag_w    = 32 - (offset_w + index_w)

  val addr_index = io.cpu_addr(index_w + offset_w - 1, offset_w)
  val addr_tag   = io.cpu_addr(31, offset_w + index_w)

  // --- Storage: VecInit registers for combinational reads ---
  val tag_arrays      = Seq.fill(c.cacheAssociativity)(RegInit(VecInit(Seq.fill(num_sets)(0.U(tag_w.W)))))
  val data_arrays     = Seq.fill(c.cacheAssociativity)(RegInit(VecInit(Seq.fill(num_sets)(0.U(c.xLen.W)))))
  val valid_bit_array = RegInit(VecInit(Seq.fill(c.cacheAssociativity)(VecInit(Seq.fill(num_sets)(false.B)))))

  // --- State Machine: IDLE and REFILL only ---
  val sIdle :: sRefill :: Nil = Enum(2)
  val state = RegInit(sIdle)

  val addr_reg        = Reg(UInt(index_w.W))
  val addr_tag_reg    = Reg(UInt(tag_w.W))
  val full_addr_reg   = Reg(UInt(32.W))
  val replacement_way = RegInit(0.U(log2Up(c.cacheAssociativity).W))

  // --- Combinational tag/data lookup ---
  // VecInit gives combinational reads: result available same cycle address presented
  val tags_out = Wire(Vec(c.cacheAssociativity, UInt(tag_w.W)))
  val data_out = Wire(Vec(c.cacheAssociativity, UInt(c.xLen.W)))
  val hits     = Wire(Vec(c.cacheAssociativity, Bool()))

  for (i <- 0 until c.cacheAssociativity) {
    tags_out(i) := tag_arrays(i)(addr_index)
    data_out(i) := data_arrays(i)(addr_index)
    hits(i)     := (tags_out(i) === addr_tag) && valid_bit_array(i)(addr_index)
  }

  val hit  = hits.asUInt.orR
  val done = io.mem.mem_valid

  // --- State Transitions ---
  switch(state) {
    is(sIdle) {
      when(io.cpu_read_en || io.cpu_write_en) {
        when(hit) {
          // Hit: serve data combinationally, stay in IDLE, no stall
          when(io.cpu_write_en) {
            for (i <- 0 until c.cacheAssociativity) {
              when(hits(i)) {
                data_arrays(i)(addr_index) := io.cpu_write_data
              }
            }
          }
        } .otherwise {
          // Miss: latch address, go to REFILL
          state         := sRefill
          addr_reg      := addr_index
          addr_tag_reg  := addr_tag
          full_addr_reg := io.cpu_addr
        }
      }
    }
    is(sRefill) {
      when(done) {
        for (i <- 0 until c.cacheAssociativity) {
          when(replacement_way === i.U) {
            tag_arrays(i)(addr_reg)      := addr_tag_reg
            data_arrays(i)(addr_reg)     := io.mem.mem_read_data
            valid_bit_array(i)(addr_reg) := true.B
          }
        }
        replacement_way := replacement_way + 1.U
        state           := sIdle
      }
    }
  }

  // --- Read data output ---
  val read_data_reg = RegInit(0.U(c.xLen.W))
  when(state === sRefill && done) {
    read_data_reg := io.mem.mem_read_data
  }
  io.cpu_read_data := Mux(state === sIdle && hit, Mux1H(hits, data_out), read_data_reg)

  // --- Stall logic ---
  // Gate requests: ignore cpu_read_en on the cycle we return to IDLE from REFILL
  val just_refilled = RegNext(state === sRefill && done, false.B)
  val request = (io.cpu_read_en || io.cpu_write_en) && !just_refilled
  val miss = (state === sIdle) && request && !hit
  io.stall_cpu := (state === sRefill) || miss

  // --- Hit/Miss Counters ---
  // Count once per request using rising edge of request signal
  val prev_request = RegNext(request, false.B)
  val new_request  = request && !prev_request
  val hit_count  = RegInit(0.U(32.W))
  val miss_count = RegInit(0.U(32.W))
  when(state === sIdle && new_request) {
    when(hit)  { hit_count  := hit_count  + 1.U }
      .otherwise { miss_count := miss_count + 1.U }
  }
  io.debug_hits   := hit_count
  io.debug_misses := miss_count

  // Memory port
  io.mem.mem_addr       := full_addr_reg
  io.mem.mem_read_en    := (state === sRefill)
  io.mem.mem_write_en   := (state === sIdle && io.cpu_write_en && hit)
  io.mem.mem_write_data := io.cpu_write_data

  io.debug_state := state.asUInt
  io.debug_rdr   := read_data_reg
}