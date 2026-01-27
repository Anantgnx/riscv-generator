package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Data_MemoryTest() extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Data_Memory"
  val c = Config()

  it should "write synchronously and read asynchronously" in {
    test(new Data_Memory(c)) { dut =>

      // -------------------------------------------------------
      // 1. Write Data (Address 4 -> Index 1)
      // -------------------------------------------------------
      dut.io.Daddress.poke(4.U)      // Address 4 (Index 1)
      dut.io.write_data.poke(1234.U) // Value to write
      dut.io.MemWrite.poke(true.B)   // Enable Write

      // Clock step required for the write to happen (Synchronous)
      dut.clock.step(1)

      // -------------------------------------------------------
      // 2. Read Back (Immediate Check)
      // -------------------------------------------------------
      dut.io.MemWrite.poke(false.B)  // Disable Write
      dut.io.Daddress.poke(4.U)      // Address 4

      // Since your Verilog uses "assign read_data = ...",
      // the data should be available IMMEDIATELY without a clock step.
      dut.io.read_data.expect(1234.U)

      // -------------------------------------------------------
      // 3. Test Write Protection (MemWrite = 0)
      // -------------------------------------------------------
      dut.io.Daddress.poke(8.U)      // Address 8 (Index 2)
      dut.io.write_data.poke(5555.U)
      dut.io.MemWrite.poke(false.B)  // Write Disabled
      dut.clock.step(1)

      // Read Address 8 -> Should be 0 (default initialized)
      dut.io.Daddress.poke(8.U)
      dut.io.read_data.expect(0.U)

      // -------------------------------------------------------
      // 4. Test Address Slicing (Byte Alignment)
      // -------------------------------------------------------
      // Writing to Address 12 (Index 3)
      dut.io.Daddress.poke(12.U)
      dut.io.write_data.poke("hDEADBEEF".U)
      dut.io.MemWrite.poke(true.B)
      dut.clock.step(1)

      // Read back
      dut.io.MemWrite.poke(false.B)
      dut.io.Daddress.poke(12.U)
      dut.io.read_data.expect("hDEADBEEF".U)
    }
  }
}