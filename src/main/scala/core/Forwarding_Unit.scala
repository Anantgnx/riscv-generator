package core

import chisel3._
import chisel3.util.log2Up

class Forwarding_Unit(c: Config) extends Module {
  val io = IO(new Bundle {
    val rs1_in = Input(UInt(log2Up(c.numRegs).W))
    val rs2_in = Input(UInt(log2Up(c.numRegs).W))
    val ex_mem_reg_write = Input(Bool())
    val ex_mem_rd = Input(UInt(log2Up(c.numRegs).W))
    val mem_wb_reg_write = Input(Bool())
    val mem_wb_rd = Input(UInt(log2Up(c.numRegs).W))
    val forward_a = Output(UInt(2.W))
    val forward_b = Output(UInt(2.W))
  })

  val f_a = WireInit(0.U(2.W))
  val f_b = WireInit(0.U(2.W))

  when(io.ex_mem_reg_write && (io.ex_mem_rd =/= 0.U) && (io.ex_mem_rd === io.rs1_in)) {
    f_a := 2.U
  } .elsewhen(io.mem_wb_reg_write && (io.mem_wb_rd =/= 0.U) && (io.mem_wb_rd === io.rs1_in)) {
    f_a := 1.U
  }

  when(io.ex_mem_reg_write && (io.ex_mem_rd =/= 0.U) && (io.ex_mem_rd === io.rs2_in)) {
    f_b := 2.U
  } .elsewhen(io.mem_wb_reg_write && (io.mem_wb_rd =/= 0.U) && (io.mem_wb_rd === io.rs2_in)) {
    f_b := 1.U
  }

  io.forward_a := f_a
  io.forward_b := f_b
}