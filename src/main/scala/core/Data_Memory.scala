package core

import chisel3._
import chisel3.util._

class DataRAM(c: Config) extends Module {
  val io        = IO(Flipped(new MemPort(c)))
  val debug_arr0  = IO(Output(UInt(c.xLen.W)))
  val debug_arr1  = IO(Output(UInt(c.xLen.W)))
  val debug_arr2  = IO(Output(UInt(c.xLen.W)))
  val debug_arr3  = IO(Output(UInt(c.xLen.W)))
  val debug_arr4  = IO(Output(UInt(c.xLen.W)))
  val debug_arr5  = IO(Output(UInt(c.xLen.W)))
  val debug_dmem0 = IO(Output(UInt(c.xLen.W)))
  val debug_dmem1 = IO(Output(UInt(c.xLen.W)))

  val mem_latency = c.memLatency.U
  val count       = RegInit(0.U(8.W))
  val is_busy     = io.mem_read_en || io.mem_write_en

  when(is_busy) {
    when(count === mem_latency) { count := 0.U }
      .otherwise                { count := count + 1.U }
  } .otherwise { count := 0.U }

  io.mem_valid := (count === mem_latency) && is_busy

  // DMEM base = 0x10000; subtract before indexing; 13-bit for 8192 words
  val word_addr = ((io.mem_addr - 0x10000.U) >> 2)(12, 0)

  def makeInit(): Vec[UInt] = {
    val init = scala.collection.mutable.ArrayBuffer.fill(8192)(BigInt(0))  // 8192 words = 32KB
    c.benchmark match {
      case 0 =>
        val A = Seq(1, 2, 3, 4); val B = Seq(5, 6, 7, 8)
        for (i <- 0 until 4) init(32 + i) = BigInt(A(i))
        for (i <- 0 until 4) init(36 + i) = BigInt(B(i))
      case 1 =>
        for (i <- 0 until 4) for (j <- 0 until 4)
          init(32 + i*4 + j) = BigInt(i*4 + j + 1)
        for (i <- 0 until 4) for (j <- 0 until 4)
          init(48 + i*4 + j) = BigInt(if (i == j) 1 else 0)
      case 2 =>
        val s = Seq(8, 7, 6, 5, 4, 3, 2, 1)
        for (i <- 0 until 8) init(32 + i) = BigInt(s(i))
      case 3 =>
        for (rep <- 0 until 4) for (v <- 0 until 8)
          init(32 + rep*8 + v) = BigInt(v)
      case 4 =>
        val s = Seq(8, 7, 6, 5, 4, 3, 2, 1)
        for (i <- 0 until 8) init(32 + i) = BigInt(s(i))
      case _ =>
    }
    VecInit(init.map(_.U(c.xLen.W)).toSeq)
  }

  val ram = RegInit(makeInit())

  // --- Read ---
  io.mem_read_data := ram(word_addr)

  // --- Write: byte-enable read-modify-write ---
  when(io.mem_write_en && io.mem_valid) {
    val old_word = ram(word_addr)
    val new_data = io.mem_write_data
    val be       = io.mem_byte_en

    val byte0 = Mux(be(0), new_data( 7,  0), old_word( 7,  0))
    val byte1 = Mux(be(1), new_data(15,  8), old_word(15,  8))
    val byte2 = Mux(be(2), new_data(23, 16), old_word(23, 16))
    val byte3 = Mux(be(3), new_data(31, 24), old_word(31, 24))

    ram(word_addr) := Cat(byte3, byte2, byte1, byte0)
  }

  // Debug ports
  debug_arr0  := ram(32.U)
  debug_arr1  := ram(33.U)
  debug_arr2  := ram(34.U)
  debug_arr3  := ram(35.U)
  debug_arr4  := ram(36.U)
  debug_arr5  := ram(37.U)
  debug_dmem0 := ram(32.U)
  debug_dmem1 := ram(33.U)
}