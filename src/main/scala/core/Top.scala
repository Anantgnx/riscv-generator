package core

import chisel3._
import chisel3.util._

class Top(c: Config) extends Module {
  val io = IO(new Bundle {
    val exit               = Output(Bool())
    val debug_pc           = Output(UInt(c.xLen.W))
    val debug_inst         = Output(UInt(c.xLen.W))
    val debug_icache_state = Output(UInt(3.W))
    val debug_dcache_state = Output(UInt(3.W))
    val debug_x10          = Output(UInt(c.xLen.W))
  })

  // 1. Interface Wires
  val stall_signal    = Wire(Bool())
  val write_back_data = Wire(UInt(c.xLen.W))

  // 2. Base Modules
  val pc_reg     = Module(new PC(c))
  val mem        = Module(new MainMemory(c))
  val regFile    = Module(new RegisterFile(c))
  val control    = Module(new Control_Unit(c))
  val immGen     = Module(new ImmGen(c))
  val alu        = Module(new ALU(c))

  val if_id      = Module(new pipeline_reg_if_id(c))
  val id_ex      = Module(new pipeline_reg_id_ex(c))
  val dcache     = Module(new Cache(c.copy(isReadOnlyCache = false)))
  val icache     = Module(new Cache(c.copy(isReadOnlyCache = true)))
  val arbiter    = Module(new core.Arbiter(c))
  val hazard     = Module(new Hazard_Unit(c))

  // --- REVISED STALL LOGIC ---
  // Only stall the PC if a cache is literally BUSY (Refilling or Comparing)
  // This allows PC to move forward once the cache hits IDLE.
  val hardware_busy = icache.io.stall_cpu || dcache.io.stall_cpu
  val hazard_stall  = if (c.isThreeStage) false.B else hazard.io.stall

  pc_reg.io.stall := hardware_busy || hazard_stall
  pc_reg.io.pc_in := Mux(pc_reg.io.stall, pc_reg.io.pc_out, pc_reg.io.pc_out + 4.U)

  // Aggressive stall ONLY for the pipeline register to prevent "Drift"
  val icache_starting = (icache.io.debug_state === 0.U) && (icache.io.cpu_read_en)
  stall_signal := hardware_busy || hazard_stall || icache_starting

  // --- IF Stage ---
  mem.io <> arbiter.io.master_mem

  icache.io.cpu_addr       := pc_reg.io.pc_out
  icache.io.cpu_read_en    := true.B
  icache.io.cpu_write_en   := false.B
  icache.io.cpu_write_data := 0.U

  arbiter.io.icache <> icache.io.mem

  if_id.io.stall          := stall_signal
  if_id.io.flush          := false.B
  if_id.io.pc_in          := pc_reg.io.pc_out

  // Only update the instruction register when the cache is NOT busy
  if_id.io.instruction_in := Mux(icache.io.stall_cpu, if_id.io.instruction_out, icache.io.cpu_read_data)

  // --- ID Stage ---
  val id_inst = if_id.io.instruction_out
  control.io.instruction := id_inst
  control.io.stall       := stall_signal

  regFile.io.ra1 := id_inst(19, 15)
  regFile.io.ra2 := id_inst(24, 20)
  immGen.io.instr := id_inst

  if (c.isThreeStage) {
    dcache.io.cpu_addr       := alu.io.alu_result.asUInt
    dcache.io.cpu_write_data := regFile.io.rd2.asUInt
    regFile.io.wd            := Mux(control.io.MemtoReg, dcache.io.cpu_read_data.asSInt, alu.io.alu_result)
    arbiter.io.dcache       <> dcache.io.mem
    dcache.io.cpu_read_en    := control.io.MemRead
    dcache.io.cpu_write_en   := control.io.Mem_write
  } else {
    // 5-STAGE LOGIC
    val ex_mem  = Module(new pipeline_reg_ex_mem(c))
    val mem_wb  = Module(new pipeline_reg_mem_wb(c))
    val forward = Module(new Forwarding_Unit(c))

    hazard.io.IF_ID_rs1     := id_inst(19, 15)
    hazard.io.IF_ID_rs2     := id_inst(24, 20)
    hazard.io.ID_EX_rd      := id_ex.io.rd_out
    hazard.io.ID_EX_MemRead := id_ex.io.mem_read_out

    forward.io.rs1_in           := id_ex.io.rs1_out
    forward.io.rs2_in           := id_ex.io.rs2_out
    forward.io.ex_mem_rd        := ex_mem.io.rd_out
    forward.io.ex_mem_reg_write := ex_mem.io.reg_write_out
    forward.io.mem_wb_rd        := mem_wb.io.rd_out
    forward.io.mem_wb_reg_write := mem_wb.io.reg_write_out

    val alu_in_a = MuxLookup(forward.io.forward_a, id_ex.io.read_data1_out)(Seq(
      1.U -> write_back_data,
      2.U -> ex_mem.io.alu_result_out
    ))

    val forward_b_mux = MuxLookup(forward.io.forward_b, id_ex.io.read_data2_out)(Seq(
      1.U -> write_back_data,
      2.U -> ex_mem.io.alu_result_out
    ))

    alu.io.op1 := alu_in_a.asSInt
    alu.io.op2 := Mux(id_ex.io.ALU_src_out, id_ex.io.sign_ext_imm_out, forward_b_mux).asSInt

    ex_mem.io.alu_result_in := alu.io.alu_result.asUInt
    ex_mem.io.write_data_in := forward_b_mux
    ex_mem.io.rd_in         := id_ex.io.rd_out
    ex_mem.io.reg_write_in  := id_ex.io.reg_write_out
    ex_mem.io.mem_write_in  := id_ex.io.mem_write_out
    ex_mem.io.mem_read_in   := id_ex.io.mem_read_out
    ex_mem.io.mem_to_reg_in := id_ex.io.mem_to_reg_out

    dcache.io.cpu_addr       := ex_mem.io.alu_result_out
    dcache.io.cpu_write_data := ex_mem.io.write_data_out
    dcache.io.cpu_read_en    := ex_mem.io.mem_read_out
    dcache.io.cpu_write_en   := ex_mem.io.mem_write_out

    arbiter.io.dcache       <> dcache.io.mem

    mem_wb.io.alu_result_in := ex_mem.io.alu_result_out
    mem_wb.io.read_data_in  := dcache.io.cpu_read_data
    mem_wb.io.rd_in         := ex_mem.io.rd_out
    mem_wb.io.reg_write_in  := ex_mem.io.reg_write_out
    mem_wb.io.mem_to_reg_in := ex_mem.io.mem_to_reg_out

    write_back_data := Mux(mem_wb.io.mem_to_reg_out, mem_wb.io.read_data_out, mem_wb.io.alu_result_out)

    regFile.io.regWrite  := mem_wb.io.reg_write_out
    regFile.io.wa        := mem_wb.io.rd_out
    regFile.io.wd        := write_back_data.asSInt
  }

  // Common
  alu.io.alu_control := id_ex.io.ALU_op_out
  io.exit := (alu.io.alu_result === 0.S) && (id_inst === 0.U)

  id_ex.io.flush            := stall_signal
  id_ex.io.pc_in            := if_id.io.pc_out
  id_ex.io.read_data1_in    := regFile.io.rd1.asUInt
  id_ex.io.read_data2_in    := regFile.io.rd2.asUInt
  id_ex.io.sign_ext_imm_in  := immGen.io.imm
  id_ex.io.rd_in            := id_inst(11, 7)
  id_ex.io.rs1_in           := id_inst(19, 15)
  id_ex.io.rs2_in           := id_inst(24, 20)
  id_ex.io.ALU_src_in       := control.io.ALU_src
  id_ex.io.ALU_op_in        := control.io.ALU_op
  id_ex.io.mem_write_in     := control.io.Mem_write
  id_ex.io.mem_read_in      := control.io.MemRead
  id_ex.io.reg_write_in     := control.io.Reg_write
  id_ex.io.mem_to_reg_in    := control.io.MemtoReg
  id_ex.io.branch_in        := control.io.Branch

  io.debug_pc           := pc_reg.io.pc_out
  io.debug_inst         := if_id.io.instruction_out
  io.debug_icache_state := icache.io.debug_state
  io.debug_dcache_state := dcache.io.debug_state
  io.debug_x10          := regFile.io.rd1.asUInt
}