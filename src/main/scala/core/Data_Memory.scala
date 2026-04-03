package core

import chisel3._
import chisel3.util._

class DataRAM(c: Config) extends Module {
  val io        = IO(Flipped(new MemPort(c)))
  val debug_arr0 = IO(Output(UInt(c.xLen.W)))  // arr[0] word32
  val debug_arr1 = IO(Output(UInt(c.xLen.W)))  // arr[1] word33
  val debug_arr2 = IO(Output(UInt(c.xLen.W)))  // arr[2] word34
  val debug_arr3 = IO(Output(UInt(c.xLen.W)))  // arr[3] word35
  val debug_arr4 = IO(Output(UInt(c.xLen.W)))  // arr[4] word36
  val debug_arr5 = IO(Output(UInt(c.xLen.W)))  // arr[5] word37
  val debug_dmem0 = IO(Output(UInt(c.xLen.W)))  // DataRAM word32 (arr[0]) direct
  val debug_dmem1 = IO(Output(UInt(c.xLen.W)))  // DataRAM word33 (arr[1]) direct

  val mem_latency = c.memLatency.U
  val count       = RegInit(0.U(8.W))
  val is_busy     = io.mem_read_en || io.mem_write_en

  when(is_busy) {
    when(count === mem_latency) { count := 0.U }
      .otherwise                { count := count + 1.U }
  } .otherwise { count := 0.U }

  io.mem_valid := (count === mem_latency) && is_busy

  val word_addr = (io.mem_addr >> 2).asUInt(8, 0)

  def makeInit(): Vec[UInt] = {
    val init = scala.collection.mutable.ArrayBuffer.fill(512)(BigInt(0))
    c.benchmark match {
      case 0 =>
        // 2x2 MatMul: A@words32-35(0x080), B@words36-39(0x090), C@words40-43(0x0A0)
        val A = Seq(1, 2, 3, 4)
        val B = Seq(5, 6, 7, 8)
        for (i <- 0 until 4) init(32 + i) = BigInt(A(i))
        for (i <- 0 until 4) init(36 + i) = BigInt(B(i))

      case 1 =>
        // 4x4 MatMul: A@words32-47(0x080), B@words48-63(0x0C0), C@words64-79(0x100)
        // A[i][j] = i*4+j+1 (1..16), B = 4x4 identity
        for (i <- 0 until 4)
          for (j <- 0 until 4)
            init(32 + i*4 + j) = BigInt(i*4 + j + 1)
        for (i <- 0 until 4)
          for (j <- 0 until 4)
            init(48 + i*4 + j) = BigInt(if (i == j) 1 else 0)
      // C@words64-79: zero (written by SW)

      case 2 =>
        // Bubble Sort: arr=[8,7,6,5,4,3,2,1] @words32-39(0x080)
        val s = Seq(8, 7, 6, 5, 4, 3, 2, 1)
        for (i <- 0 until 8) init(32 + i) = BigInt(s(i))

      case 3 =>
        // Histogram: 32 inputs [0,1,2,3,4,5,6,7] x4 @words32-63(0x080)
        // Bins @words64-71(0x100): zero-initialized
        for (rep <- 0 until 4)
          for (v <- 0 until 8) init(32 + rep*8 + v) = BigInt(v)

      case 4 =>
        // Write-through test: same layout as bench2
        val s = Seq(8, 7, 6, 5, 4, 3, 2, 1)
        for (i <- 0 until 8) init(32 + i) = BigInt(s(i))

      case _ =>
    }
    VecInit(init.map(_.U(c.xLen.W)).toSeq)
  }

  val ram = RegInit(makeInit())

  io.mem_read_data := ram(word_addr)

  when(io.mem_write_en && io.mem_valid) {
    ram(word_addr) := io.mem_write_data
  }

  // Debug ports: arr[0..5] at words 32-37 (0x080-0x094)
  debug_arr0 := ram(32.U)
  debug_arr1 := ram(33.U)
  debug_arr2 := ram(34.U)
  debug_arr3 := ram(35.U)
  debug_arr4 := ram(36.U)
  debug_arr5 := ram(37.U)
  debug_dmem0 := ram(32.U)
  debug_dmem1 := ram(33.U)
}