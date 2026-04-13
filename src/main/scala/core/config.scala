package core

import chisel3._

case class Config(
                   xLen             : Int     = 32,
                   numRegs          : Int     = 32,
                   isThreeStage     : Boolean = false,
                   hasMul           : Boolean = false,
                   hasDiv           : Boolean = false,
                   mulLatency       : Int     = 1,
                   hasCache         : Boolean = false,
                   cacheAssociativity: Int    = 1,
                   cacheSizeKB      : Int     = 1,
                   numSets          : Option[Int] = None,  // overrides cacheSizeKB if set
                   memLatency       : Int     = 10,
                   benchmark        : Int     = 0,
                   hexFile          : String  = ""  // path to hex file for benchmark >= 9
                 )

class MemPort(c: Config) extends Bundle {
  val mem_addr       = Output(UInt(c.xLen.W))
  val mem_read_data  = Input(UInt(c.xLen.W))
  val mem_write_data = Output(UInt(c.xLen.W))
  val mem_write_en   = Output(Bool())
  val mem_read_en    = Output(Bool())
  val mem_byte_en    = Output(UInt(4.W))  // byte enables: bit3=byte3 ... bit0=byte0
  val mem_valid      = Input(Bool())
}