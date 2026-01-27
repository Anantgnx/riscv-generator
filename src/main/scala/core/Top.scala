package core

import chisel3._
import chisel3.util._
import chisel3.util.log2Up

class Top(c: Config) extends Module {
  val io = IO(new Bundle {
    val exit = Output(Bool())
  })

  // 1. Placeholder for data coming from the end of the pipeline
  val write_back_data = Wire(UInt(c.xLen.W))

  // 2. Instantiate all modules
  val pc_reg     = Module(new PC(c))
  val imem       = Module(new InstructionMemory(c))
  val regFile    = Module(new RegisterFile(c))
  val control    = Module(new Control_Unit(c))
  val immGen     = Module(new ImmGen(c))
  val alu        = Module(new ALU(c))
  val dmem       = Module(new Data_Memory(c))

  val if_id      = Module(new pipeline_reg_if_id(c))
  val id_ex      = Module(new pipeline_reg_id_ex(c))
  val ex_mem     = Module(new pipeline_reg_ex_mem(c))
  val mem_wb     = Module(new pipeline_reg_mem_wb(c))

  val hazard     = Module(new Hazard_Unit(c))
  val forward    = Module(new Forwarding_Unit(c))

  // IF

  val pc_plus_4 = pc_reg.io.pc_out + 4.U

  pc_reg.io.stall := hazard.io.stall
  pc_reg.io.pc_in := pc_plus_4

  imem.io.address := pc_reg.io.pc_out

  if_id.io.stall          := hazard.io.stall
  if_id.io.flush          := false.B
  if_id.io.pc_in          := pc_reg.io.pc_out
  if_id.io.instruction_in := imem.io.data_out

  // ID

  val id_inst = if_id.io.instruction_out

  control.io.instruction := id_inst
  control.io.stall       := hazard.io.stall

  regFile.io.ra1 := id_inst(19, 15)
  regFile.io.ra2 := id_inst(24, 20)
  immGen.io.instr := id_inst

  hazard.io.IF_ID_rs1     := id_inst(19, 15)
  hazard.io.IF_ID_rs2     := id_inst(24, 20)
  hazard.io.ID_EX_rd      := id_ex.io.rd_out
  hazard.io.ID_EX_MemRead := id_ex.io.mem_read_out

  id_ex.io.flush            := hazard.io.stall
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

  // EX

  forward.io.rs1_in           := id_ex.io.rs1_out
  forward.io.rs2_in           := id_ex.io.rs2_out
  forward.io.ex_mem_rd        := ex_mem.io.rd_out
  forward.io.ex_mem_reg_write := ex_mem.io.reg_write_out
  forward.io.mem_wb_rd        := mem_wb.io.rd_out
  forward.io.mem_wb_reg_write := mem_wb.io.reg_write_out

  val alu_in_a = MuxLookup(forward.io.forward_a, id_ex.io.read_data1_out)(Seq(
    0.U -> id_ex.io.read_data1_out,
    1.U -> write_back_data,
    2.U -> ex_mem.io.alu_result_out
  ))

  val forward_b_mux = MuxLookup(forward.io.forward_b, id_ex.io.read_data2_out)(Seq(
    0.U -> id_ex.io.read_data2_out,
    1.U -> write_back_data,
    2.U -> ex_mem.io.alu_result_out
  ))

  alu.io.op1         := alu_in_a.asSInt
  alu.io.op2         := Mux(id_ex.io.ALU_src_out, id_ex.io.sign_ext_imm_out, forward_b_mux).asSInt
  alu.io.alu_control := id_ex.io.ALU_op_out

  ex_mem.io.alu_result_in := alu.io.alu_result.asUInt
  ex_mem.io.write_data_in := forward_b_mux
  ex_mem.io.rd_in         := id_ex.io.rd_out
  ex_mem.io.reg_write_in  := id_ex.io.reg_write_out
  ex_mem.io.mem_write_in  := id_ex.io.mem_write_out
  ex_mem.io.mem_read_in   := id_ex.io.mem_read_out
  ex_mem.io.mem_to_reg_in := id_ex.io.mem_to_reg_out

  // MEM
  dmem.io.Daddress   := ex_mem.io.alu_result_out
  dmem.io.write_data := ex_mem.io.write_data_out
  dmem.io.MemRead    := ex_mem.io.mem_read_out
  dmem.io.MemWrite   := ex_mem.io.mem_write_out

  mem_wb.io.alu_result_in := ex_mem.io.alu_result_out
  mem_wb.io.read_data_in  := dmem.io.read_data
  mem_wb.io.rd_in         := ex_mem.io.rd_out
  mem_wb.io.reg_write_in  := ex_mem.io.reg_write_out
  mem_wb.io.mem_to_reg_in := ex_mem.io.mem_to_reg_out

  // WB

  write_back_data := Mux(mem_wb.io.mem_to_reg_out, mem_wb.io.read_data_out, mem_wb.io.alu_result_out)

  regFile.io.regWrite  := mem_wb.io.reg_write_out
  regFile.io.wa        := mem_wb.io.rd_out
  regFile.io.wd        := write_back_data.asSInt

  io.exit := false.B
}