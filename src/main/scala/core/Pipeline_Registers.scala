package core

import chisel3._
import chisel3.util.log2Up

class pipeline_reg_if_id(c: Config) extends Module {
  val io = IO(new Bundle {
    val stall           = Input(Bool())
    val flush           = Input(Bool())
    val pc_in           = Input(UInt(c.xLen.W))
    val instruction_in  = Input(UInt(c.xLen.W))
    val pc_out          = Output(UInt(c.xLen.W))
    val instruction_out = Output(UInt(c.xLen.W))
  })

  val pc_reg   = RegInit(0.U(c.xLen.W))
  val inst_reg = RegInit(0.U(c.xLen.W))

  when(io.flush) {
    pc_reg   := 0.U
    inst_reg := 0.U
  } .elsewhen(!io.stall) {
    pc_reg   := io.pc_in
    inst_reg := io.instruction_in
  }

  io.pc_out          := pc_reg
  io.instruction_out := inst_reg
}

class pipeline_reg_id_ex(c: Config) extends Module {
  val io = IO(new Bundle {
    val stall            = Input(Bool())
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
    val funct3_in        = Input(UInt(3.W))

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
    val funct3_out       = Output(UInt(3.W))
  })

  val pc      = RegInit(0.U(c.xLen.W))
  val rd1     = RegInit(0.U(c.xLen.W))
  val rd2     = RegInit(0.U(c.xLen.W))
  val imm     = RegInit(0.U(c.xLen.W))
  val rd      = RegInit(0.U(log2Up(c.numRegs).W))
  val rs1     = RegInit(0.U(log2Up(c.numRegs).W))
  val rs2     = RegInit(0.U(log2Up(c.numRegs).W))
  val alu_src = RegInit(false.B)
  val alu_op  = RegInit(0.U(4.W))
  val mem_w   = RegInit(false.B)
  val mem_r   = RegInit(false.B)
  val reg_w   = RegInit(false.B)
  val m2r     = RegInit(false.B)
  val br      = RegInit(false.B)
  val funct3  = RegInit(0.U(3.W))

  when(io.flush) {
    pc := 0.U; rd1 := 0.U; rd2 := 0.U; imm := 0.U
    rd := 0.U; rs1 := 0.U; rs2 := 0.U
    alu_src := false.B; alu_op := 0.U
    mem_w := false.B; mem_r := false.B
    reg_w := false.B; m2r := false.B
    br := false.B; funct3 := 0.U
  } .elsewhen(!io.stall) {
    pc      := io.pc_in
    rd1     := io.read_data1_in
    rd2     := io.read_data2_in
    imm     := io.sign_ext_imm_in
    rd      := io.rd_in
    rs1     := io.rs1_in
    rs2     := io.rs2_in
    alu_src := io.ALU_src_in
    alu_op  := io.ALU_op_in
    mem_w   := io.mem_write_in
    mem_r   := io.mem_read_in
    reg_w   := io.reg_write_in
    m2r     := io.mem_to_reg_in
    br      := io.branch_in
    funct3  := io.funct3_in
  }

  io.pc_out           := pc
  io.read_data1_out   := rd1
  io.read_data2_out   := rd2
  io.sign_ext_imm_out := imm
  io.rd_out           := rd
  io.rs1_out          := rs1
  io.rs2_out          := rs2
  io.ALU_src_out      := alu_src
  io.ALU_op_out       := alu_op
  io.mem_write_out    := mem_w
  io.mem_read_out     := mem_r
  io.reg_write_out    := reg_w
  io.mem_to_reg_out   := m2r
  io.branch_out       := br
  io.funct3_out       := funct3
}

// ── EX/MEM: added funct3 ──────────────────────────────────────────────────
class pipeline_reg_ex_mem(c: Config) extends Module {
  val io = IO(new Bundle {
    val stall          = Input(Bool())
    val alu_result_in  = Input(UInt(c.xLen.W))
    val write_data_in  = Input(UInt(c.xLen.W))
    val rd_in          = Input(UInt(log2Up(c.numRegs).W))
    val mem_read_in    = Input(Bool())
    val mem_write_in   = Input(Bool())
    val reg_write_in   = Input(Bool())
    val mem_to_reg_in  = Input(Bool())
    val funct3_in      = Input(UInt(3.W))   // NEW

    val alu_result_out = Output(UInt(c.xLen.W))
    val write_data_out = Output(UInt(c.xLen.W))
    val rd_out         = Output(UInt(log2Up(c.numRegs).W))
    val mem_read_out   = Output(Bool())
    val mem_write_out  = Output(Bool())
    val reg_write_out  = Output(Bool())
    val mem_to_reg_out = Output(Bool())
    val funct3_out     = Output(UInt(3.W))  // NEW
  })

  val alu_result = RegInit(0.U(c.xLen.W))
  val write_data = RegInit(0.U(c.xLen.W))
  val rd         = RegInit(0.U(log2Up(c.numRegs).W))
  val mem_r      = RegInit(false.B)
  val mem_w      = RegInit(false.B)
  val reg_w      = RegInit(false.B)
  val m2r        = RegInit(false.B)
  val funct3     = RegInit(0.U(3.W))        // NEW

  when(!io.stall) {
    alu_result := io.alu_result_in
    write_data := io.write_data_in
    rd         := io.rd_in
    mem_r      := io.mem_read_in
    mem_w      := io.mem_write_in
    reg_w      := io.reg_write_in
    m2r        := io.mem_to_reg_in
    funct3     := io.funct3_in              // NEW
  }

  io.alu_result_out := alu_result
  io.write_data_out := write_data
  io.rd_out         := rd
  io.mem_read_out   := mem_r
  io.mem_write_out  := mem_w
  io.reg_write_out  := reg_w
  io.mem_to_reg_out := m2r
  io.funct3_out     := funct3               // NEW
}

// ── MEM/WB: added funct3 ─────────────────────────────────────────────────
class pipeline_reg_mem_wb(c: Config) extends Module {
  val io = IO(new Bundle {
    val stall          = Input(Bool())
    val read_data_in   = Input(UInt(c.xLen.W))
    val alu_result_in  = Input(UInt(c.xLen.W))
    val rd_in          = Input(UInt(log2Up(c.numRegs).W))
    val reg_write_in   = Input(Bool())
    val mem_to_reg_in  = Input(Bool())
    val funct3_in      = Input(UInt(3.W))   // NEW

    val read_data_out  = Output(UInt(c.xLen.W))
    val alu_result_out = Output(UInt(c.xLen.W))
    val rd_out         = Output(UInt(log2Up(c.numRegs).W))
    val reg_write_out  = Output(Bool())
    val mem_to_reg_out = Output(Bool())
    val funct3_out     = Output(UInt(3.W))  // NEW
  })

  val read_data  = RegInit(0.U(c.xLen.W))
  val alu_result = RegInit(0.U(c.xLen.W))
  val rd         = RegInit(0.U(log2Up(c.numRegs).W))
  val reg_w      = RegInit(false.B)
  val m2r        = RegInit(false.B)
  val funct3     = RegInit(0.U(3.W))        // NEW

  when(!io.stall) {
    read_data  := io.read_data_in
    alu_result := io.alu_result_in
    rd         := io.rd_in
    reg_w      := io.reg_write_in
    m2r        := io.mem_to_reg_in
    funct3     := io.funct3_in              // NEW
  }

  io.read_data_out  := read_data
  io.alu_result_out := alu_result
  io.rd_out         := rd
  io.reg_write_out  := reg_w
  io.mem_to_reg_out := m2r
  io.funct3_out     := funct3               // NEW
}