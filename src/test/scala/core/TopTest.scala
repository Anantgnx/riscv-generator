package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TopTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pipelined Top Level"
  val c = Config()

  it should "execute an ADDI instruction through the pipeline" in {
    test(new Top(c)) { dut =>
      // ADDI x1, x0, 10  (0x00A00093)
      // We need to wait for it to pass through IF, ID, EX, MEM, and finally WB

      // Cycle 1: Fetch (IF)
      dut.clock.step(1)

      // Cycle 2: Decode (ID)
      dut.clock.step(1)

      // Cycle 3: Execute (EX)
      dut.clock.step(1)

      // Cycle 4: Memory (MEM)
      dut.clock.step(1)

      // Cycle 5: Write Back (WB) - Value 10 should be written to x1 here
      dut.clock.step(1)

      // Peek into the Register File (if you made it accessible, or check via next instruction)
      // For now, let's assume you have a way to verify regFile(1) == 10
      // In a real test, you'd usually load a small program into InstructionMemory
    }
  }

  it should "verify forwarding logic (RAW hazard)" in {
    test(new Top(c)) { dut =>
      /* Program:
         ADDI x1, x0, 10
         ADDI x2, x1, 5   <-- Needs forwarding from x1
      */

      // This requires your InstructionMemory to be pre-loaded with these hex values.
      // If your InstructionMemory is hardcoded, you'll need to update it with:
      // 0: 00A00093 (addi x1, x0, 10)
      // 4: 00508113 (addi x2, x1, 5)

      dut.clock.step(10) // Let the pipeline drain

      // After 10 cycles, x2 should be 15 if forwarding worked.
    }
  }
}