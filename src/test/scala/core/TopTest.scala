package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TopTest extends AnyFlatSpec with ChiselScalatestTester {

  val cfg = Config(benchmark = 1)
  val maxCycles = 500

  "RISCV Processor" should "run benchmark" in {
    test(new Top(cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      println("=" * 70)
      println(s"Benchmark : Vector Reduction (benchmark=1)")
      println(s"Config    : cacheSizeKB=${cfg.cacheSizeKB} assoc=${cfg.cacheAssociativity} latency=${cfg.memLatency} stages=5")
      println("=" * 70)

      var cycles = 0
      while (cycles < maxCycles) {
        val pc      = dut.io.debug_pc.peek().litValue
        val inst    = dut.io.debug_inst.peek().litValue
        val x10     = dut.io.debug_x10.peek().litValue
        val x15     = dut.io.debug_x15.peek().litValue
        val rdr     = dut.io.debug_rdr.peek().litValue
        val em_m2r  = dut.io.debug_em_m2r.peek().litValue
        val hb      = dut.io.debug_hb.peek().litValue
        val hz      = dut.io.debug_hz.peek().litValue
        val mw_rw   = dut.io.debug_mw_rw.peek().litValue
        val mw_rd   = dut.io.debug_mw_rd.peek().litValue
        val mwrdat  = dut.io.debug_mwrdat.peek().litValue
        val x12     = dut.io.debug_x12.peek().litValue
        val dcSt    = dut.io.debug_dcache_state.peek().litValue.toInt
        val dcStr   = dcSt match {
          case 0 => "IDLE  "
          case 1 => "REFILL"
          case _ => "UNKNWN"
        }

        println(f"Cycle $cycles%4d | PC: 0x$pc%04x | x15: $x15%4d | mwrd: $mwrdat%4d | em_m2r: $em_m2r | mw_rw: $mw_rw | mw_rd: $mw_rd%2d | hb: $hb | hz: $hz | DC: $dcStr")

        dut.clock.step(1)
        cycles += 1
      }

      val finalX10 = dut.io.debug_x10.peek().litValue
      val hits     = dut.io.debug_hits.peek().litValue
      val misses   = dut.io.debug_misses.peek().litValue
      val total    = hits + misses
      val hitRate  = if (total > 0) hits.toDouble / total.toDouble * 100.0 else 0.0

      println("=" * 70)
      println(f"Final x10      : $finalX10")
      println(f"D-Cache hits   : $hits")
      println(f"D-Cache misses : $misses")
      println(f"Hit rate       : $hitRate%.1f%%")
      println("=" * 70)
    }
  }
}