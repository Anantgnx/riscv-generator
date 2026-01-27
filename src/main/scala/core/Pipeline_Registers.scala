package core

import chisel3._
import chisel3.util.log2Up

class pipeline_reg_if_id(c: Config) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val flush = Input(Bool())
    val pc_in = Input(UInt(c.xLen.W))
    val instruction_in = Input(UInt(c.xLen.W))
    val pc_out = Output(UInt(c.xLen.W))
    val instruction_out = Output(UInt(c.xLen.W))
  })

  val pc_out_reg = RegInit(0.U(c.xLen.W))
  val instruction_out_reg = RegInit(0.U(c.xLen.W))

  when(io.stall) {
    pc_out_reg := pc_out_reg
    instruction_out_reg := instruction_out_reg
  }
  .elsewhen(io.flush) {
    pc_out_reg := 0.U
    instruction_out_reg := 0.U
  }
  .elsewhen(!io.stall) {
    pc_out_reg := io.pc_in
    instruction_out_reg := io.instruction_in
  }

  io.pc_out := pc_out_reg
  io.instruction_out := instruction_out_reg
}

class pipeline_reg_id_ex(c: Config) extends Module {
  val io = IO(new Bundle {
    val flush            = Input(Bool())
    val pc_in            = Input(UInt(c.xLen.W))
    val read_data1_in    = Input(UInt(c.xLen.W))
    val read_data2_in    = Input(UInt(c.xLen.W))
    val sign_ext_imm_in  = Input(UInt(c.xLen.W))
    val rd_in            = Input(UInt(log2Up(c.numRegs).W))
    val rs1_in           = Input(UInt(log2Up(c.numRegs).W))
    val rs2_in           = Input(UInt(log2Up(c.numRegs).W))
    val ALU_src_in       = Input(Bool())
    val ALU_op_in        = Input(UInt(4.W))
    val mem_write_in     = Input(Bool())
    val mem_read_in      = Input(Bool())
    val reg_write_in     = Input(Bool())
    val mem_to_reg_in    = Input(Bool())
    val branch_in        = Input(Bool())

    val pc_out           = Output(UInt(c.xLen.W))
    val read_data1_out   = Output(UInt(c.xLen.W))
    val read_data2_out   = Output(UInt(c.xLen.W))
    val sign_ext_imm_out = Output(UInt(c.xLen.W))
    val rd_out           = Output(UInt(log2Up(c.numRegs).W))
    val rs1_out          = Output(UInt(log2Up(c.numRegs).W))
    val rs2_out          = Output(UInt(log2Up(c.numRegs).W))
    val ALU_src_out      = Output(Bool())
    val ALU_op_out       = Output(UInt(4.W))
    val mem_write_out    = Output(Bool())
    val mem_read_out     = Output(Bool())
    val reg_write_out    = Output(Bool())
    val mem_to_reg_out   = Output(Bool())
    val branch_out       = Output(Bool())
  })

  val pc           = RegInit(0.U(c.xLen.W))
  val rd1          = RegInit(0.U(c.xLen.W))
  val rd2          = RegInit(0.U(c.xLen.W))
  val imm          = RegInit(0.U(c.xLen.W))
  val rd           = RegInit(0.U(log2Up(c.numRegs).W))
  val rs1          = RegInit(0.U(log2Up(c.numRegs).W))
  val rs2          = RegInit(0.U(log2Up(c.numRegs).W))
  val alu_src      = RegInit(false.B)
  val alu_op       = RegInit(0.U(4.W))
  val mem_w        = RegInit(false.B)
  val mem_r        = RegInit(false.B)
  val reg_w        = RegInit(false.B)
  val m2r          = RegInit(false.B)
  val br           = RegInit(false.B)

  when(io.flush) {
    pc := 0.U; rd1 := 0.U; rd2 := 0.U; imm := 0.U; rd := 0.U; rs1 := 0.U; rs2 := 0.U
    alu_src := false.B; alu_op := 0.U; mem_w := false.B; mem_r := false.B; reg_w := false.B
    m2r := false.B; br := false.B
  } .otherwise {
    pc := io.pc_in; rd1 := io.read_data1_in; rd2 := io.read_data2_in; imm := io.sign_ext_imm_in
    rd := io.rd_in; rs1 := io.rs1_in; rs2 := io.rs2_in; alu_src := io.ALU_src_in
    alu_op := io.ALU_op_in; mem_w := io.mem_write_in; mem_r := io.mem_read_in
    reg_w := io.reg_write_in; m2r := io.mem_to_reg_in; br := io.branch_in
  }

  io.pc_out := pc; io.read_data1_out := rd1; io.read_data2_out := rd2; io.sign_ext_imm_out := imm
  io.rd_out := rd; io.rs1_out := rs1; io.rs2_out := rs2; io.ALU_src_out := alu_src
  io.ALU_op_out := alu_op; io.mem_write_out := mem_w; io.mem_read_out := mem_r
  io.reg_write_out := reg_w; io.mem_to_reg_out := m2r; io.branch_out := br
}

class pipeline_reg_ex_mem(c: Config) extends Module {
  val io = IO(new Bundle {
    val alu_result_in  = Input(UInt(c.xLen.W))
    val write_data_in  = Input(UInt(c.xLen.W))
    val rd_in          = Input(UInt(log2Up(c.numRegs).W))
    val mem_read_in    = Input(Bool())
    val mem_write_in   = Input(Bool())
    val reg_write_in   = Input(Bool())
    val mem_to_reg_in  = Input(Bool())

    val alu_result_out = Output(UInt(c.xLen.W))
    val write_data_out = Output(UInt(c.xLen.W))
    val rd_out         = Output(UInt(log2Up(c.numRegs).W))
    val mem_read_out   = Output(Bool())
    val mem_write_out  = Output(Bool())
    val reg_write_out  = Output(Bool())
    val mem_to_reg_out = Output(Bool())
  })

  io.alu_result_out := RegNext(io.alu_result_in, 0.U)
  io.write_data_out := RegNext(io.write_data_in, 0.U)
  io.rd_out         := RegNext(io.rd_in, 0.U)
  io.mem_read_out   := RegNext(io.mem_read_in, false.B)
  io.mem_write_out  := RegNext(io.mem_write_in, false.B)
  io.reg_write_out  := RegNext(io.reg_write_in, false.B)
  io.mem_to_reg_out := RegNext(io.mem_to_reg_in, false.B)
}

class pipeline_reg_mem_wb(c: Config) extends Module {
  val io = IO(new Bundle {
    val read_data_in   = Input(UInt(c.xLen.W))
    val alu_result_in  = Input(UInt(c.xLen.W))
    val rd_in          = Input(UInt(log2Up(c.numRegs).W))
    val reg_write_in   = Input(Bool())
    val mem_to_reg_in  = Input(Bool())

    val read_data_out  = Output(UInt(c.xLen.W))
    val alu_result_out = Output(UInt(c.xLen.W))
    val rd_out         = Output(UInt(log2Up(c.numRegs).W))
    val reg_write_out  = Output(Bool())
    val mem_to_reg_out = Output(Bool())
  })

  io.read_data_out  := RegNext(io.read_data_in, 0.U)
  io.alu_result_out := RegNext(io.alu_result_in, 0.U)
  io.rd_out         := RegNext(io.rd_in, 0.U)
  io.reg_write_out  := RegNext(io.reg_write_in, false.B)
  io.mem_to_reg_out := RegNext(io.mem_to_reg_in, false.B)
}