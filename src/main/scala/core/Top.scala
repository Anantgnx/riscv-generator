package core

import chisel3._
import chisel3.util._

class Top(c: Config) extends Module {
  val io = IO(new Bundle {
    val exit               = Output(Bool())
    val debug_pc           = Output(UInt(c.xLen.W))
    val debug_dcache_state = Output(UInt(3.W))
    val debug_x1           = Output(UInt(c.xLen.W))
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
    val debug_hb           = Output(Bool())
    val debug_hz           = Output(Bool())
    val debug_hits         = Output(UInt(32.W))
    val debug_misses       = Output(UInt(32.W))
    val debug_arr0         = Output(UInt(32.W))
    val debug_arr1         = Output(UInt(32.W))
    val debug_arr2         = Output(UInt(32.W))
    val debug_arr3         = Output(UInt(32.W))
    val debug_arr4         = Output(UInt(32.W))
    val debug_arr5         = Output(UInt(32.W))
    val debug_dmem0        = Output(UInt(32.W))
    val debug_dmem1        = Output(UInt(32.W))
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
  irom.io.pc             := pc_reg.io.pc_out
  if_id.io.stall         := hardware_busy || hazard_stall
  if_id.io.pc_in         := pc_reg.io.pc_out
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
    dcache.io.cpu_byte_en    := "b1111".U  // full word (3-stage)
    dataRam.io               <> dcache.io.mem
    regFile.io.wd            := Mux(control.io.MemtoReg, dcache.io.cpu_read_data.asSInt, alu.io.alu_result)

    io.debug_hb := hardware_busy
    io.debug_hz := false.B

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

    // AUIPC(13), JAL(9), JALR(7): use PC as op1 so ALU computes PC+imm or PC+4
    val need_pc_as_op1 = (id_ex.io.ALU_op_out === 13.U) ||
      (id_ex.io.ALU_op_out === 9.U)  ||
      (id_ex.io.ALU_op_out === 7.U)
    alu.io.op1 := Mux(need_pc_as_op1, id_ex.io.pc_out.asSInt, alu_in_a.asSInt)
    alu.io.op2 := Mux(id_ex.io.ALU_src_out, id_ex.io.sign_ext_imm_out, forward_b_mux).asSInt

    // Branch taken logic for all 6 branch types
    // BEQ(000): SUB result=0   → taken if zero
    // BNE(001): SUB result!=0  → taken if not-zero
    // BLT(100): SLT result=1   → taken if result=1
    // BGE(101): SLT result=0   → taken if result=0
    // BLTU(110): SLTU result=1 → taken if result=1
    // BGEU(111): SLTU result=0 → taken if result=0
    val branch_funct3 = id_ex.io.funct3_out
    val branch_result = alu.io.alu_result
    val branch_cond = MuxLookup(branch_funct3, false.B)(Seq(
      "b000".U -> (branch_result === 0.S),   // BEQ
      "b001".U -> (branch_result =/= 0.S),   // BNE
      "b100".U -> (branch_result === 1.S),   // BLT
      "b101".U -> (branch_result === 0.S),   // BGE
      "b110".U -> (branch_result === 1.S),   // BLTU
      "b111".U -> (branch_result === 0.S)    // BGEU
    ))
    val branch_taken = id_ex.io.branch_out && branch_cond
    val branch_target = (id_ex.io.pc_out.asSInt + id_ex.io.sign_ext_imm_out.asSInt).asUInt

    // JAL: PC = PC + J-imm (same formula as branch_target)
    val jal_taken  = (id_ex.io.ALU_op_out === 9.U)
    val jal_target = branch_target  // PC + J-imm

    // JALR: PC = (rs1 + imm) & ~1
    val jalr_taken  = (id_ex.io.ALU_op_out === 7.U)
    val jalr_target = ((alu_in_a.asSInt + id_ex.io.sign_ext_imm_out.asSInt).asUInt & ~1.U(32.W))

    val any_jump = branch_taken || jal_taken || jalr_taken

    pc_reg.io.stall := hardware_busy || hazard_stall
    pc_reg.io.pc_in := Mux(
      jalr_taken,  jalr_target,
      Mux(jal_taken,    jal_target,
        Mux(branch_taken, branch_target,
          Mux(pc_reg.io.stall, pc_reg.io.pc_out, pc_reg.io.pc_out + 4.U))))

    if_id.io.flush := any_jump
    id_ex.io.stall := hardware_busy
    id_ex.io.flush := (hazard_stall && !hardware_busy) || any_jump

    ex_mem.io.stall         := hardware_busy
    ex_mem.io.alu_result_in  := alu.io.alu_result.asUInt
    ex_mem.io.write_data_in  := forward_b_mux
    ex_mem.io.rd_in          := id_ex.io.rd_out
    ex_mem.io.reg_write_in   := id_ex.io.reg_write_out
    ex_mem.io.mem_write_in   := id_ex.io.mem_write_out
    ex_mem.io.mem_read_in    := id_ex.io.mem_read_out
    ex_mem.io.mem_to_reg_in  := id_ex.io.mem_to_reg_out
    ex_mem.io.funct3_in      := id_ex.io.funct3_out

    dcache.io.cpu_addr       := ex_mem.io.alu_result_out
    dcache.io.cpu_write_data := ex_mem.io.write_data_out
    dcache.io.cpu_read_en  := ex_mem.io.mem_read_out
    dcache.io.cpu_write_en := ex_mem.io.mem_write_out
    dataRam.io             <> dcache.io.mem

    // Byte enables and write data alignment from funct3 + byte offset
    val mem_byte_off = ex_mem.io.alu_result_out(1, 0)  // addr[1:0]
    val mem_funct3   = ex_mem.io.funct3_out
    val raw_write    = ex_mem.io.write_data_out

    // SB: select exactly which byte lane based on offset
    // SH: select upper or lower halfword lanes
    // SW: all 4 lanes
    val byte_en = Wire(UInt(4.W))
    byte_en := "b1111".U  // default: SW
    when(mem_funct3 === "b000".U) {  // SB
      when(mem_byte_off === 0.U) { byte_en := "b0001".U }
        .elsewhen(mem_byte_off === 1.U) { byte_en := "b0010".U }
        .elsewhen(mem_byte_off === 2.U) { byte_en := "b0100".U }
        .otherwise                      { byte_en := "b1000".U }
    }.elsewhen(mem_funct3 === "b001".U) {  // SH
      when(mem_byte_off(1)) { byte_en := "b1100".U }
        .otherwise             { byte_en := "b0011".U }
    }
    dcache.io.cpu_byte_en := byte_en

    // Replicate write data so the correct byte lane has the value
    val aligned_write = Wire(UInt(c.xLen.W))
    aligned_write := raw_write  // default: SW
    when(mem_funct3 === "b000".U) {  // SB: replicate byte to all lanes
      aligned_write := Fill(4, raw_write(7, 0))
    }.elsewhen(mem_funct3 === "b001".U) {  // SH: replicate half to both lanes
      aligned_write := Cat(raw_write(15, 0), raw_write(15, 0))
    }
    dcache.io.cpu_write_data := aligned_write

    mem_wb.io.stall         := hardware_busy
    mem_wb.io.alu_result_in := ex_mem.io.alu_result_out
    mem_wb.io.read_data_in  := dcache.io.cpu_read_data
    mem_wb.io.rd_in         := ex_mem.io.rd_out
    mem_wb.io.reg_write_in  := ex_mem.io.reg_write_out
    mem_wb.io.mem_to_reg_in := ex_mem.io.mem_to_reg_out
    mem_wb.io.funct3_in     := ex_mem.io.funct3_out

    // Load data extraction: select byte/half and sign/zero extend
    val load_word    = mem_wb.io.read_data_out
    val load_funct3  = mem_wb.io.funct3_out
    // byte offset was in the ALU result — but at WB we only have the full word
    // Use alu_result[1:0] which passed through mem_wb as alu_result_out
    val load_byte_off = mem_wb.io.alu_result_out(1, 0)
    val load_byte = MuxLookup(load_byte_off, load_word(7, 0))(Seq(
      0.U -> load_word( 7,  0),
      1.U -> load_word(15,  8),
      2.U -> load_word(23, 16),
      3.U -> load_word(31, 24)
    ))
    val load_half = Mux(load_byte_off(1), load_word(31, 16), load_word(15, 0))

    val load_data = MuxLookup(load_funct3, load_word)(Seq(
      "b000".U -> Cat(Fill(24, load_byte(7)),  load_byte),  // LB:  sign extend
      "b001".U -> Cat(Fill(16, load_half(15)), load_half),  // LH:  sign extend
      "b010".U -> load_word,                                 // LW:  full word
      "b100".U -> Cat(0.U(24.W), load_byte),                // LBU: zero extend
      "b101".U -> Cat(0.U(16.W), load_half)                 // LHU: zero extend
    ))

    write_back_data := Mux(mem_wb.io.mem_to_reg_out, load_data, mem_wb.io.alu_result_out)
    regFile.io.regWrite  := mem_wb.io.reg_write_out
    regFile.io.wa        := mem_wb.io.rd_out
    regFile.io.wd        := write_back_data.asSInt

    io.debug_hb := hardware_busy
    io.debug_hz := hazard_stall
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
  io.debug_dcache_state := dcache.io.debug_state
  regFile.io.ra1        := id_inst(19, 15)

  io.debug_x1  := regFile.io.debug_x1
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
  io.debug_arr0   := dcache.io.debug_arr0
  io.debug_arr1   := dcache.io.debug_arr1
  io.debug_arr2   := dcache.io.debug_arr2
  io.debug_arr3   := dcache.io.debug_arr3
  io.debug_arr4   := dcache.io.debug_arr4
  io.debug_arr5   := dcache.io.debug_arr5
  io.debug_dmem0  := dataRam.debug_dmem0
  io.debug_dmem1  := dataRam.debug_dmem1
}