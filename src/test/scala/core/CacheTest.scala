package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CacheSpec extends AnyFlatSpec with ChiselScalatestTester {
  val testConfig = Config(xLen = 32, cacheSizeKB = 4, cacheAssciativity = 2)

  "Cache" should "handle a basic Read Miss followed by a Read Hit" in {
    test(new Cache(testConfig)) { dut =>
      dut.io.cpu_addr.poke()
    }
  }
}