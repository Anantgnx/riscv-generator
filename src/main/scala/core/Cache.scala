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
    val mem_write_en = Output(Bool())
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

  switch(state) {
    is(sIdle) {
      state := sLookup
    }

    is(sLookup) {
      state := sCompare
    }

    is(sCompare) {
      when(hit) {
        state := sIdle
      }.otherwise {
        state := sRefill
      }
    }

    is(sRefill) {
      when(done) {
        state := sIdle
      }
    }
  }

  val addr_reg = Reg(UInt(index_w.W))
  val addr_tag_reg = Reg(UInt(tag_w.W))
  when(state === sIdle){
    addr_reg := addr_index
    addr_tag_reg := addr_tag
  }

  val sram_index = Mux(state === sIdle, addr_index, addr_reg)
  io.stall_cpu := (state === sLookup || state === sCompare || state === sRefill)

  val tags_out = Wire(Vec(c.cacheAssociativity, UInt(tag_w.W)))
  val tags_match = Wire(Vec(c.cacheAssociativity, Bool()))
  val valid = Wire(Vec(c.cacheAssociativity, Bool()))
  val hits = Wire(Vec(c.cacheAssociativity, Bool()))
  for (i <- 0 until c.cacheAssociativity) {
    tags_out(i) := tag_arrays(i).read(sram_index)

    tags_match(i) := tags_out(i) === addr_tag_reg
    valid(i) := valid_bit_array(i)(addr_reg)

    hits(i) := tags_match(i) && valid(i)
  }

  val hit = hits.asUInt.orR

}