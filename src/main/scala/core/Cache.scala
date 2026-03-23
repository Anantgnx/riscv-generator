package core

import chisel3._
import chisel3.util._
import chisel3.util.log2Up

class Cache(c: Config) extends Module {
  val io = IO(new Bundle {
    val cpu_addr = Input(UInt(c.xLen.W))
    val cpu_write_data = Input(UInt(c.xLen.W))
    val cpu_read_en = Input(Bool())
    val cpu_write_en = Input(Bool())
    val cpu_read_data = Output(UInt(c.xLen.W))
    val stall_cpu = Output(Bool())

    val mem_addr = Output((UInt(c.xLen.W)))
    val mem_read_data = Input(UInt(c.xLen.W))
    val mem_write_data = Output(UInt(c.xLen.W))
    val mem_write_en = Output(Bool())
    val mem_valid = Input(Bool())
    val mem_read_en = Output(Bool())
  })

  val num_sets = ((c.cacheSizeKB*1024)/(c.xLen/8)/c.cacheAssociativity)
  val index_w = log2Up(num_sets)
  val offset_w = log2Up(c.xLen/8)
  val tag_w = 32 - (offset_w + index_w)

  val addr_index = io.cpu_addr(index_w + offset_w - 1, offset_w)
  val addr_tag = io.cpu_addr(31, offset_w + index_w)

  val tag_arrays = Seq.fill(c.cacheAssociativity) (
    SyncReadMem(num_sets, UInt(tag_w.W))
  )

  val data_arrays = Seq.fill(c.cacheAssociativity)(
    SyncReadMem(num_sets, UInt(c.xLen.W))
  )

  val valid_bit_array = RegInit(VecInit(Seq.fill(c.cacheAssociativity)(VecInit(Seq.fill(num_sets)(false.B)))))

  val sIdle :: sLookup :: sCompare :: sRefill :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val done = io.mem_valid
  val replacement_way = RegInit(0.U(log2Up(c.cacheAssociativity).W))

  val addr_reg = Reg(UInt(index_w.W))
  val addr_tag_reg = Reg(UInt(tag_w.W))
  val full_addr_reg = Reg(UInt(32.W))

  switch(state) {
    is(sIdle) {
      // Only transition if the CPU is actually requesting a memory op
      when(io.cpu_read_en || io.cpu_write_en) {
        state := sLookup
        addr_reg := addr_index
        addr_tag_reg := addr_tag
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
            // Update cache on write-hit (Write-Through)
            for (i <- 0 until c.cacheAssociativity) {
              when(hits(i)) {
                data_arrays(i).write(addr_reg, io.cpu_write_data)
              }
            }
          }
        }
        state := sIdle
      }.otherwise {
        if (!c.isReadOnlyCache) {
          // D-Cache logic:
          // If Write-Miss -> Idle (Write-No-Allocate)
          // If Read-Miss -> Refill
          state := Mux(io.cpu_write_en, sIdle, sRefill)
        } else {
          // I-Cache logic: Always refill on miss
          state := sRefill
        }
      }
    }

    is(sRefill) {
      when(done) {
        for (i <- 0 until c.cacheAssociativity) {
          when(i.U === replacement_way) {
            tag_arrays(i).write(addr_reg, addr_tag_reg)
            data_arrays(i).write(addr_reg, io.mem_read_data)
            valid_bit_array(i)(addr_reg) := true.B
          }
        }
        replacement_way := replacement_way + 1.U
        state := sIdle
      }
    }
  }

  val sram_index = Mux(state === sIdle, addr_index, addr_reg)
  io.stall_cpu := (state === sLookup || state === sCompare || (state === sRefill && !done))

  val tags_out = Wire(Vec(c.cacheAssociativity, UInt(tag_w.W)))
  val hits = Wire(Vec(c.cacheAssociativity, Bool()))
  val data_out = Wire(Vec(c.cacheAssociativity, UInt(c.xLen.W)))

  for (i <- 0 until c.cacheAssociativity) {
    tags_out(i) := tag_arrays(i).read(sram_index)
    data_out(i) := data_arrays(i).read(sram_index)

    hits(i) := (tags_out(i) === addr_tag_reg) && valid_bit_array(i)(addr_reg)
  }

  val hit = hits.asUInt.orR

  // CPU and Memory Data/Control outputs
  io.cpu_read_data  := Mux1H(hits, data_out)
  io.mem_addr       := full_addr_reg
  io.mem_write_en   := (!c.isReadOnlyCache.B && state === sCompare && io.cpu_write_en)
  io.mem_write_data := io.cpu_write_data
  io.mem_read_en := (state === sRefill) || (state === sCompare && !hit && io.cpu_read_en)
}