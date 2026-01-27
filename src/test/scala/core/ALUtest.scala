package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ALUTest() extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ALU"
  val c = Config()

  it should "perform ADD operation correctly" in {
    test(new ALU(c)) { dut =>
      // Test 1: 10 + 20 = 30
      // Note: Inputs are SInt (Signed), so we use .S
      dut.io.op1.poke(10.S)
      dut.io.op2.poke(20.S)
      dut.io.alu_control.poke("b00000".U) // Set this to your ADD opcode

      dut.clock.step(1)

      // Result is UInt (Unsigned) based on your error logs
      dut.io.alu_result.expect(30.S)
    }
  }

  it should "perform SUB operation correctly" in {
    test(new ALU(c)) { dut =>
      // Test 2: 20 - 10 = 10
      dut.io.op1.poke(20.S)
      dut.io.op2.poke(10.S)
      dut.io.alu_control.poke("b0001".U) // Set this to your SUB opcode

      dut.clock.step(1)

      dut.io.alu_result.expect(10.S)
      dut.io.zero.expect(false.B)
    }
  }

  it should "assert Zero flag when result is 0" in {
    test(new ALU(c)) { dut =>
      // Test 3: 10 - 10 = 0
      dut.io.op1.poke(10.S)
      dut.io.op2.poke(10.S)
      dut.io.alu_control.poke("b0001".U) // SUB opcode

      dut.clock.step(1)

      dut.io.alu_result.expect(0.S)
      dut.io.zero.expect(true.B)
    }
  }
}