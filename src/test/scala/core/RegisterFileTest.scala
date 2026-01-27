package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class RegisterFileTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "RegisterFile"
  val c = Config()

  it should "write to a register (x1) and read it back" in {
    test(new RegisterFile(c)) { dut =>
      // 1. Write -123 to register x1
      dut.io.wa.poke(1.U)
      dut.io.wd.poke(-123.S)
      dut.io.regWrite.poke(true.B)

      dut.clock.step(1)

      // 2. Disable Write
      dut.io.regWrite.poke(false.B)

      // 3. Read x1 on port 1
      dut.io.ra1.poke(1.U)
      dut.io.rd1.expect(-123.S)

      // 4. Verify port 2 can also read it
      dut.io.ra2.poke(1.U)
      dut.io.rd2.expect(-123.S)
    }
  }

  it should "maintain x0 as hardwired zero" in {
    test(new RegisterFile(c)) { dut =>
      // Try to write 999 to x0
      dut.io.wa.poke(0.U)
      dut.io.wd.poke(999.S)
      dut.io.regWrite.poke(true.B)

      dut.clock.step(1)

      // Read x0
      dut.io.ra1.poke(0.U)
      dut.io.rd1.expect(0.S)
    }
  }

  it should "not write when regWrite is false" in {
    test(new RegisterFile(c)) { dut =>
      // Try to write to x2 without enabling write
      dut.io.wa.poke(2.U)
      dut.io.wd.poke(555.S)
      dut.io.regWrite.poke(false.B)

      dut.clock.step(1)

      // Read x2 (should stay 0)
      dut.io.ra1.poke(2.U)
      dut.io.rd1.expect(0.S)
    }
  }
}