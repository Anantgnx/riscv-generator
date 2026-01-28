package core

import chisel3._

object RiscvGenerator extends App {
  val c = Config(xLen = 32, numRegs = 32)

  // Use the built-in emitVerilog helper
  // This avoids the "Unknown option" flag errors entirely
  val verilogString = getVerilogString(new Top(c))

  // This prints the Verilog to your console so you can see it's real
  println(verilogString)

  // This writes it to a file explicitly using standard Java/Scala IO
  val out = new java.io.PrintWriter("Top.v")
  out.print(verilogString)
  out.close()
}