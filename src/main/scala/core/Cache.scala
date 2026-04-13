package core

import chisel3._
import chisel3.util._

class Cache(c: Config) extends Module {
  val io = IO(new Bundle {
    val cpu_addr = Input(UInt(c.xLen.W))
    val cpu_write_data = Input(UInt(c.xLen.W))
    val cpu_read_en = Input(Bool())
    val cpu_write_en  = Input(Bool())
    val cpu_byte_en   = Input(UInt(4.W))   // byte enables from pipeline
    val cpu_read_data = Output(UInt(c.xLen.W))
    val stall_cpu = Output(Bool())
    val debug_state = Output(UInt(3.W))
    val debug_hits   = Output(UInt(32.W))
    val debug_misses = Output(UInt(32.W))
    val debug_arr0   = Output(UInt(32.W))
    val debug_arr1   = Output(UInt(32.W))
    val debug_arr2   = Output(UInt(32.W))
    val debug_arr3   = Output(UInt(32.W))
    val debug_arr4   = Output(UInt(32.W))
    val debug_arr5   = Output(UInt(32.W))
    val mem = new MemPort(c)
  })

  // --- Parameters & Address Decoding ---
  // numSets overrides cacheSizeKB calculation for sub-1KB caches
  val num_sets = c.numSets.getOrElse((c.cacheSizeKB * 1024) / (c.xLen / 8) / c.cacheAssociativity)
  val index_w = log2Up(num_sets)
  val offset_w = log2Up(c.xLen / 8)
  val tag_w = 32 - (offset_w + index_w)

  val addr_index = io.cpu_addr(index_w + offset_w - 1, offset_w)
  val addr_tag = io.cpu_addr(31, offset_w + index_w)

  // --- Storage: VecInit registers for combinational reads ---
  val tag_arrays = Seq.fill(c.cacheAssociativity)(RegInit(VecInit(Seq.fill(num_sets)(0.U(tag_w.W)))))
  val data_arrays = Seq.fill(c.cacheAssociativity)(RegInit(VecInit(Seq.fill(num_sets)(0.U(c.xLen.W)))))
  val valid_bit_array = RegInit(VecInit(Seq.fill(c.cacheAssociativity)(VecInit(Seq.fill(num_sets)(false.B)))))

  // --- State Machine: IDLE and REFILL only ---
  val sIdle :: sRefill :: Nil = Enum(2)
  val state = RegInit(sIdle)

  val addr_reg = Reg(UInt(index_w.W))
  val addr_tag_reg = Reg(UInt(tag_w.W))
  val full_addr_reg = Reg(UInt(32.W))
  val write_data_reg = Reg(UInt(c.xLen.W))  // latched write data for write-hit writeback
  val byte_en_reg    = RegInit("b1111".U(4.W)) // latched byte enables, captured when entering sRefill
  val write_hit_reg = RegInit(false.B)       // true when sRefill is serving a write-hit writeback
  val replacement_way = RegInit(0.U(log2Up(c.cacheAssociativity).W))

  // --- Combinational tag/data lookup ---
  val tags_out = Wire(Vec(c.cacheAssociativity, UInt(tag_w.W)))
  val data_out = Wire(Vec(c.cacheAssociativity, UInt(c.xLen.W)))
  val hits = Wire(Vec(c.cacheAssociativity, Bool()))

  for (i <- 0 until c.cacheAssociativity) {
    tags_out(i) := tag_arrays(i)(addr_index)
    data_out(i) := data_arrays(i)(addr_index)
    hits(i) := (tags_out(i) === addr_tag) && valid_bit_array(i)(addr_index)
  }

  val hit = hits.asUInt.orR
  val done = io.mem.mem_valid

  // --- State Transitions ---
  switch(state) {
    is(sIdle) {
      when(io.cpu_read_en || io.cpu_write_en) {
        when(hit) {
          when(io.cpu_write_en) {
            when(io.cpu_byte_en === "b1111".U) {
              // Full word write-hit: update cache and write-through to DataRAM
              for (i <- 0 until c.cacheAssociativity) {
                when(hits(i)) {
                  data_arrays(i)(addr_index) := io.cpu_write_data
                }
              }
              state          := sRefill
              full_addr_reg  := io.cpu_addr
              write_data_reg := io.cpu_write_data
              byte_en_reg    := io.cpu_byte_en
              write_hit_reg  := true.B
            }.otherwise {
              // Partial write-hit (SB/SH): invalidate cache line so next
              // read refills from DataRAM which has the correct merged value
              for (i <- 0 until c.cacheAssociativity) {
                when(hits(i)) {
                  valid_bit_array(i)(addr_index) := false.B
                }
              }
              // Still write-through to DataRAM with byte enables
              state          := sRefill
              full_addr_reg  := io.cpu_addr
              write_data_reg := io.cpu_write_data
              byte_en_reg    := io.cpu_byte_en
              write_hit_reg  := true.B
            }
          }
        }.otherwise {
          // Read or write miss: enter sRefill to fetch from DataRAM
          state         := sRefill
          addr_reg      := addr_index
          addr_tag_reg  := addr_tag
          full_addr_reg := io.cpu_addr
          write_hit_reg := false.B
        }
      }
    }
    is(sRefill) {
      when(done) {
        when(!write_hit_reg) {
          // Normal refill: update cache with data from DataRAM
          for (i <- 0 until c.cacheAssociativity) {
            when(replacement_way === i.U) {
              tag_arrays(i)(addr_reg)        := addr_tag_reg
              data_arrays(i)(addr_reg)       := io.mem.mem_read_data
              valid_bit_array(i)(addr_reg)   := true.B
            }
          }
          replacement_way := replacement_way + 1.U
        }
        // write_hit_reg=true: DataRAM write-through complete, cache already updated
        write_hit_reg := false.B
        state         := sIdle
      }
    }
  }

  // --- Read data output ---
  val read_data_reg = RegInit(0.U(c.xLen.W))
  when(state === sRefill && done && !write_hit_reg) {
    read_data_reg := io.mem.mem_read_data
  }
  io.cpu_read_data := Mux(state === sIdle && hit, Mux1H(hits, data_out), read_data_reg)

  // --- Stall logic ---
  // Stall during sRefill (both read-miss refill and write-hit writeback)
  val just_refilled = RegNext(state === sRefill && done, false.B)
  val request = (io.cpu_read_en || io.cpu_write_en) && !just_refilled
  val miss = (state === sIdle) && request && !hit
  io.stall_cpu := (state === sRefill) || miss

  // --- Hit/Miss Counters ---
  val prev_request = RegNext(request, false.B)
  val new_request = request && !prev_request
  val hit_count = RegInit(0.U(32.W))
  val miss_count = RegInit(0.U(32.W))
  when(state === sIdle && new_request) {
    when(hit) { hit_count := hit_count + 1.U }
      .otherwise { miss_count := miss_count + 1.U }
  }
  io.debug_hits := hit_count
  io.debug_misses := miss_count

  // Memory port
  io.mem.mem_addr      := full_addr_reg
  io.mem.mem_read_en    := (state === sRefill) && !write_hit_reg
  io.mem.mem_write_en   := (state === sRefill) && (write_hit_reg || io.cpu_write_en)
  io.mem.mem_write_data := Mux(write_hit_reg, write_data_reg, io.cpu_write_data)
  io.mem.mem_byte_en    := byte_en_reg     // use registered value (captured on sRefill entry)

  io.debug_state := state.asUInt

  // Debug: arr[0..5] live from cache data array (way 0, indices 32-37)
  io.debug_arr0 := data_arrays(0)(32.U)
  io.debug_arr1 := data_arrays(0)(33.U)
  io.debug_arr2 := data_arrays(0)(34.U)
  io.debug_arr3 := data_arrays(0)(35.U)
  io.debug_arr4 := data_arrays(0)(36.U)
  io.debug_arr5 := data_arrays(0)(37.U)
}