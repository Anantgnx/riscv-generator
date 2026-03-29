package core

import chisel3._
import chisel3.util._

class Top(c: Config) extends Module {
  val io = IO(new Bundle {
    val exit               = Output(Bool())
    val debug_pc           = Output(UInt(c.xLen.W))
    val debug_inst         = Output(UInt(c.xLen.W))
    val debug_dcache_state = Output(UInt(3.W))
    // x10–x24 debug ports
    val debug_x10          = Output(UInt(c.xLen.W))
    val debug_x11          = Output(UInt(c.xLen.W))
    val debug_x12          = Output(UInt(c.xLen.W))
    val debug_x13          = Output(UInt(c.xLen.W))
    val debug_x14          = Output(UInt(c.xLen.W))
    val debug_x15          = Output(UInt(c.xLen.W))
    val debug_x16          = Output(UInt(c.xLen.W))
    val debug_x17          = Output(UInt(c.xLen.W))
    val debug_x18          = Output(UInt(c.xLen.W))
    val debug_x19          = Output(UInt(c.xLen.W))
    val debug_x20          = Output(UInt(c.xLen.W))
    val debug_x21          = Output(UInt(c.xLen.W))
    val debug_x22          = Output(UInt(c.xLen.W))
    val debug_x23          = Output(UInt(c.xLen.W))
    val debug_x24          = Output(UInt(c.xLen.W))
    val debug_hits         = Output(UInt(32.W))
    val debug_misses       = Output(UInt(32.W))
    val debug_rdr          = Output(UInt(32.W))
    val debug_hb           = Output(Bool())
    val debug_hz           = Output(Bool())
    val debug_em_m2r       = Output(Bool())
    val debug_mw_rw        = Output(Bool())
    val debug_mw_rd        = Output(UInt(5.W))
    val debug_mwrdat       = Output(UInt(32.W))
  })

  val write_back_data = WireDefault(0.U(c.xLen.W))

  // --- Modules ---
  val pc_reg  = Module(new PC(c))
  val irom    = Module(new InstructionROM(c))
  val dataRam = Module(new DataRAM(c))
  val regFile = Module(new RegisterFile(c))
  val control = Module(new Control_Unit(c))
  val immGen  = Module(new ImmGen(c))
  val alu     = Module(new ALU(c))
  val if_id   = Module(new pipeline_reg_if_id(c))
  val id_ex   = Module(new pipeline_reg_id_ex(c))
  val dcache  = Module(new Cache(c))
  val hazard  = Module(new Hazard_Unit(c))

  val hardware_busy = dcache.io.stall_cpu
  val hazard_stall  = if (c.isThreeStage) false.B else hazard.io.stall

  // --- IF Stage ---
  irom.io.pc      := pc_reg.io.pc_out
  if_id.io.stall  := hardware_busy || hazard_stall
  if_id.io.pc_in  := pc_reg.io.pc_out
  if_id.io.instruction_in := irom.io.inst_out

  // --- ID Stage ---
  val id_inst = if_id.io.instruction_out
  control.io.instruction := id_inst
  control.io.stall       := hardware_busy || hazard_stall
  regFile.io.ra2         := id_inst(24, 20)
  immGen.io.instr        := id_inst

  if (c.isThreeStage) {
    pc_reg.io.stall := hardware_busy
    pc_reg.io.pc_in := Mux(hardware_busy, pc_reg.io.pc_out, pc_reg.io.pc_out + 4.U)
    if_id.io.flush  := false.B
    id_ex.io.stall  := false.B
    id_ex.io.flush  := false.B

    dcache.io.cpu_addr       := alu.io.alu_result.asUInt
    dcache.io.cpu_write_data := regFile.io.rd2.asUInt
    dcache.io.cpu_read_en    := control.io.MemRead
    dcache.io.cpu_write_en   := control.io.Mem_write
    dataRam.io               <> dcache.io.mem
    regFile.io.wd            := Mux(control.io.MemtoReg, dcache.io.cpu_read_data.asSInt, alu.io.alu_result)

    io.debug_hb      := hardware_busy
    io.debug_hz      := false.B
    io.debug_em_m2r  := false.B
    io.debug_mw_rw   := false.B
    io.debug_mw_rd   := 0.U
    io.debug_mwrdat  := 0.U

  } else {
    // --- 5-STAGE ---
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

    val branch_taken = id_ex.io.branch_out && Mux(
      id_ex.io.funct3_out === "b001".U,
      alu.io.alu_result =/= 0.S,
      alu.io.alu_result === 0.S
    )
    val branch_target = (id_ex.io.pc_out.asSInt + id_ex.io.sign_ext_imm_out.asSInt).asUInt

    pc_reg.io.stall := hardware_busy || hazard_stall
    pc_reg.io.pc_in := Mux(
      branch_taken,
      branch_target,
      Mux(pc_reg.io.stall, pc_reg.io.pc_out, pc_reg.io.pc_out + 4.U)
    )

    if_id.io.flush := branch_taken
    id_ex.io.stall := hardware_busy
    id_ex.io.flush := (hazard_stall && !hardware_busy) || branch_taken

    ex_mem.io.stall         := hardware_busy
    ex_mem.io.alu_result_in  := alu.io.alu_result.asUInt
    ex_mem.io.write_data_in  := forward_b_mux
    ex_mem.io.rd_in          := id_ex.io.rd_out
    ex_mem.io.reg_write_in   := id_ex.io.reg_write_out
    ex_mem.io.mem_write_in   := id_ex.io.mem_write_out
    ex_mem.io.mem_read_in    := id_ex.io.mem_read_out
    ex_mem.io.mem_to_reg_in  := id_ex.io.mem_to_reg_out

    dcache.io.cpu_addr       := ex_mem.io.alu_result_out
    dcache.io.cpu_write_data := ex_mem.io.write_data_out
    dcache.io.cpu_read_en    := ex_mem.io.mem_read_out
    dcache.io.cpu_write_en   := ex_mem.io.mem_write_out
    dataRam.io               <> dcache.io.mem

    mem_wb.io.stall         := hardware_busy
    mem_wb.io.alu_result_in := ex_mem.io.alu_result_out
    mem_wb.io.read_data_in  := dcache.io.cpu_read_data
    mem_wb.io.rd_in         := ex_mem.io.rd_out
    mem_wb.io.reg_write_in  := ex_mem.io.reg_write_out
    mem_wb.io.mem_to_reg_in := ex_mem.io.mem_to_reg_out

    write_back_data      := Mux(mem_wb.io.mem_to_reg_out, mem_wb.io.read_data_out, mem_wb.io.alu_result_out)
    regFile.io.regWrite  := mem_wb.io.reg_write_out
    regFile.io.wa        := mem_wb.io.rd_out
    regFile.io.wd        := write_back_data.asSInt

    io.debug_hb      := hardware_busy
    io.debug_hz      := hazard_stall
    io.debug_em_m2r  := ex_mem.io.mem_to_reg_out
    io.debug_mw_rw   := mem_wb.io.reg_write_out
    io.debug_mw_rd   := mem_wb.io.rd_out
    io.debug_mwrdat  := mem_wb.io.read_data_out
  }

  // Common
  alu.io.alu_control := id_ex.io.ALU_op_out
  io.exit            := false.B

  id_ex.io.pc_in           := if_id.io.pc_out
  id_ex.io.read_data1_in   := regFile.io.rd1.asUInt
  id_ex.io.read_data2_in   := regFile.io.rd2.asUInt
  id_ex.io.sign_ext_imm_in := immGen.io.imm
  id_ex.io.rd_in           := id_inst(11, 7)
  id_ex.io.rs1_in          := id_inst(19, 15)
  id_ex.io.rs2_in          := id_inst(24, 20)
  id_ex.io.ALU_src_in      := control.io.ALU_src
  id_ex.io.ALU_op_in       := control.io.ALU_op
  id_ex.io.mem_write_in    := control.io.Mem_write
  id_ex.io.mem_read_in     := control.io.MemRead
  id_ex.io.reg_write_in    := control.io.Reg_write
  id_ex.io.mem_to_reg_in   := control.io.MemtoReg
  id_ex.io.branch_in       := control.io.Branch
  id_ex.io.funct3_in       := control.io.funct3

  io.debug_pc           := pc_reg.io.pc_out
  io.debug_inst         := if_id.io.instruction_out
  io.debug_dcache_state := dcache.io.debug_state
  regFile.io.ra1        := id_inst(19, 15)

  // Register file debug outputs x10–x24
  io.debug_x10 := regFile.io.debug_x10
  io.debug_x11 := regFile.io.debug_x11
  io.debug_x12 := regFile.io.debug_x12
  io.debug_x13 := regFile.io.debug_x13
  io.debug_x14 := regFile.io.debug_x14
  io.debug_x15 := regFile.io.debug_x15
  io.debug_x16 := regFile.io.debug_x16
  io.debug_x17 := regFile.io.debug_x17
  io.debug_x18 := regFile.io.debug_x18
  io.debug_x19 := regFile.io.debug_x19
  io.debug_x20 := regFile.io.debug_x20
  io.debug_x21 := regFile.io.debug_x21
  io.debug_x22 := regFile.io.debug_x22
  io.debug_x23 := regFile.io.debug_x23
  io.debug_x24 := regFile.io.debug_x24

  io.debug_hits   := dcache.io.debug_hits
  io.debug_misses := dcache.io.debug_misses
  io.debug_rdr    := dcache.io.debug_rdr
}