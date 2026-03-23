package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CacheSpec extends AnyFlatSpec with ChiselScalatestTester {
  val testConfig = Config(xLen = 32, cacheSizeKB = 4, cacheAssociativity = 2)

  "Cache" should "handle a basic Read Miss followed by a Read Hit" in {
    test(new Cache(testConfig)) { dut =>
      dut.io.cpu_addr.poke("h10".U)
      dut.io.cpu_read_en.poke(true.B)
      dut.clock.step()

      dut.io.stall_cpu.expect(true.B)

      while (dut.state.peek().litValue != 3) {
        dut.clock.step()
      }

      dut.io.mem_read_data.poke("hDEADBEEF".U)
      dut.io.mem_valid.poke(true.B)
      dut.clock.step()

      dut.io.stall_cpu.expect(false.B)
      dut.state.expect(0.U)

      dut.io.mem_valid.poke(false.B)
      dut.clock.step(2)

      dut.io.stall_cpu.expect(false.B)
      dut.io.cpu_read_data.expect("hDEADBEEF".U)
    }
  }


}