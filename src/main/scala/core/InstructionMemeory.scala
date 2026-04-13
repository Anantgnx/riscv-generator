package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline

// Instruction ROM — benchmark selected via c.benchmark
//
// bench0: 2x2 Matrix Multiply  A=[[1,2],[3,4]] B=[[5,6],[7,8]]
//         Expected: x10 = C[1][1] = 50
//         Data: A@0x080, B@0x090, C@0x0A0
//
// bench1: 4x4 Matrix Multiply  A=[[1..16]], B=identity
//         Expected: x10 = C[3][3] = 16
//         Data: A@0x080(words32-47), B@0x0C0(words48-63), C@0x100(words64-79)
//
// bench2: Bubble Sort  arr=[8,7,6,5,4,3,2,1] at 0x080
//         Expected: x10 = arr[0] = 1
//
// bench3: Histogram  32 inputs [0..7] x4 at 0x080, bins at 0x100
//         Expected: x10 = bins[0] = 4

class InstructionROM(c: Config) extends Module {
  val io = IO(new Bundle {
    val pc       = Input(UInt(c.xLen.W))
    val inst_out = Output(UInt(c.xLen.W))
  })

  // ---------------------------------------------------------------
  // BENCHMARK 0: 2x2 Matrix Multiply
  // ---------------------------------------------------------------
  val bench0 = VecInit(Seq(
    "h00000593".U(32.W), // 0x00: addi x11,x0,0      # i_off=0
    "h00800713".U(32.W), // 0x04: addi x14,x0,8      # inner limit=8
    "h08000793".U(32.W), // 0x08: addi x15,x0,128    # base_A=0x80
    "h09000813".U(32.W), // 0x0C: addi x16,x0,144    # base_B=0x90
    "h0A000893".U(32.W), // 0x10: addi x17,x0,160    # base_C=0xA0
    "h00000613".U(32.W), // 0x14: addi x12,x0,0      # j_off=0  LOOP_I
    "h00000913".U(32.W), // 0x18: addi x18,x0,0      # acc=0    LOOP_J
    "h00000693".U(32.W), // 0x1C: addi x13,x0,0      # k_off=0
    "h00B78AB3".U(32.W), // 0x20: add x21,x15,x11    # LOOP_K: addr=bA+i_off
    "h00DA8AB3".U(32.W), // 0x24: add x21,x21,x13    # addr+=k_off
    "h000AA983".U(32.W), // 0x28: lw x19,0(x21)      # A[i][k]
    "h00D68B33".U(32.W), // 0x2C: add x22,x13,x13    # k_off*2
    "h01680AB3".U(32.W), // 0x30: add x21,x16,x22    # addr=bB+k_off*2
    "h00CA8AB3".U(32.W), // 0x34: add x21,x21,x12    # addr+=j_off
    "h000AAA03".U(32.W), // 0x38: lw x20,0(x21)      # B[k][j]
    "h034989B3".U(32.W), // 0x3C: mul x19,x19,x20    # A[i][k]*B[k][j]
    "h01390933".U(32.W), // 0x40: add x18,x18,x19    # acc+=product
    "h00468693".U(32.W), // 0x44: addi x13,x13,4     # k_off+=4
    "h00E6ABB3".U(32.W), // 0x48: slt x23,x13,x14    # k_off<8?
    "hFC0B9AE3".U(32.W), // 0x4C: bne x23,x0,-44     # goto LOOP_K
    "h00B88AB3".U(32.W), // 0x50: add x21,x17,x11    # addr=bC+i_off
    "h00CA8AB3".U(32.W), // 0x54: add x21,x21,x12    # addr+=j_off
    "h012AA023".U(32.W), // 0x58: sw x18,0(x21)      # C[i][j]=acc
    "h00460613".U(32.W), // 0x5C: addi x12,x12,4     # j_off+=4
    "h00E62BB3".U(32.W), // 0x60: slt x23,x12,x14    # j_off<8?
    "hFA0B9AE3".U(32.W), // 0x64: bne x23,x0,-76     # goto LOOP_J
    "h00858593".U(32.W), // 0x68: addi x11,x11,8     # i_off+=8
    "h01000C13".U(32.W), // 0x6C: addi x24,x0,16     # outer limit=16
    "h0185ABB3".U(32.W), // 0x70: slt x23,x11,x24    # i_off<16?
    "hFA0B90E3".U(32.W), // 0x74: bne x23,x0,-96     # goto LOOP_I
    "h00C8A503".U(32.W), // 0x78: lw x10,12(x17)     # x10=C[1][1]=50
    "h00000013".U(32.W)  // 0x7C: nop
  ))

  // ---------------------------------------------------------------
  // BENCHMARK 1: 4x4 Matrix Multiply
  // A=[[1..4],[5..8],[9..12],[13..16]], B=identity -> C=A, x10=C[3][3]=16
  // A@0x080(words32-47), B@0x0C0(words48-63), C@0x100(words64-79)
  // ---------------------------------------------------------------
  val bench1 = VecInit(Seq(
    "h00000593".U(32.W), // 0x00: addi x11,x0,0      # i_off=0
    "h01000713".U(32.W), // 0x04: addi x14,x0,16     # inner limit=16
    "h08000793".U(32.W), // 0x08: addi x15,x0,128    # base_A=0x80
    "h0C000813".U(32.W), // 0x0C: addi x16,x0,192    # base_B=0xC0
    "h10000893".U(32.W), // 0x10: addi x17,x0,256    # base_C=0x100
    "h00000613".U(32.W), // 0x14: addi x12,x0,0      # j_off=0  LOOP_I
    "h00000913".U(32.W), // 0x18: addi x18,x0,0      # acc=0    LOOP_J
    "h00000693".U(32.W), // 0x1C: addi x13,x0,0      # k_off=0
    "h00B78AB3".U(32.W), // 0x20: add x21,x15,x11    # LOOP_K: addr=bA+i_off
    "h00DA8AB3".U(32.W), // 0x24: add x21,x21,x13    # addr+=k_off
    "h000AA983".U(32.W), // 0x28: lw x19,0(x21)      # A[i][k]
    "h00D68B33".U(32.W), // 0x2C: add x22,x13,x13    # x22=k_off*2
    "h016B0B33".U(32.W), // 0x30: add x22,x22,x22    # x22=k_off*4
    "h01680AB3".U(32.W), // 0x34: add x21,x16,x22    # addr=bB+k_off*4
    "h00CA8AB3".U(32.W), // 0x38: add x21,x21,x12    # addr+=j_off
    "h000AAA03".U(32.W), // 0x3C: lw x20,0(x21)      # B[k][j]
    "h034989B3".U(32.W), // 0x40: mul x19,x19,x20    # A[i][k]*B[k][j]
    "h01390933".U(32.W), // 0x44: add x18,x18,x19    # acc+=product
    "h00468693".U(32.W), // 0x48: addi x13,x13,4     # k_off+=4
    "h00E6ABB3".U(32.W), // 0x4C: slt x23,x13,x14    # k_off<16?
    "hFC0B98E3".U(32.W), // 0x50: bne x23,x0,-48     # goto LOOP_K
    "h00B88AB3".U(32.W), // 0x54: add x21,x17,x11    # addr=bC+i_off
    "h00CA8AB3".U(32.W), // 0x58: add x21,x21,x12    # addr+=j_off
    "h012AA023".U(32.W), // 0x5C: sw x18,0(x21)      # C[i][j]=acc
    "h00460613".U(32.W), // 0x60: addi x12,x12,4     # j_off+=4
    "h00E62BB3".U(32.W), // 0x64: slt x23,x12,x14    # j_off<16?
    "hFA0B98E3".U(32.W), // 0x68: bne x23,x0,-80     # goto LOOP_J
    "h01058593".U(32.W), // 0x6C: addi x11,x11,16    # i_off+=16
    "h04000C13".U(32.W), // 0x70: addi x24,x0,64     # outer limit=64
    "h0185ABB3".U(32.W), // 0x74: slt x23,x11,x24    # i_off<64?
    "hF80B9EE3".U(32.W), // 0x78: bne x23,x0,-100    # goto LOOP_I
    "h03C8A503".U(32.W), // 0x7C: lw x10,60(x17)     # x10=C[3][3]=16
    "h00000013".U(32.W), // 0x80: nop
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W), // pad
    "h00000013".U(32.W)  // pad (64 total)
  ))

  // ---------------------------------------------------------------
  // BENCHMARK 2: Bubble Sort (8 elements)
  // arr=[8,7,6,5,4,3,2,1] at 0x080, expected x10=arr[0]=1
  // Fix: addi x14,x0,7 is INSIDE outer loop so inner counter resets each pass
  // ---------------------------------------------------------------
  val bench2 = VecInit(Seq(
    "h08000593".U(32.W), // 0x00: addi x11,x0,128    # base=0x80
    "h00700613".U(32.W), // 0x04: addi x12,x0,7      # outer=7
    "h00058693".U(32.W), // 0x08: addi x13,x11,0     # ptr=base   OUTER
    "h00700713".U(32.W), // 0x0C: addi x14,x0,7      # inner=7 (reset each outer pass)
    "h0006A783".U(32.W), // 0x10: lw x15,0(x13)      # arr[i]     INNER
    "h0046A803".U(32.W), // 0x14: lw x16,4(x13)      # arr[i+1]
    "h00F828B3".U(32.W), // 0x18: slt x17,x16,x15    # arr[i+1]<arr[i]?
    "h00088863".U(32.W), // 0x1C: beq x17,x0,+16     # no swap → skip both SWs to 0x2C
    "h0106A023".U(32.W), // 0x20: sw x16,0(x13)      # arr[i]=arr[i+1]
    "h00F6A223".U(32.W), // 0x24: sw x15,4(x13)      # arr[i+1]=arr[i]
    "h00468693".U(32.W), // 0x28: addi x13,x13,4     # ptr++
    "hFFF70713".U(32.W), // 0x2C: addi x14,x14,-1    # inner--
    "h00000013".U(32.W), // 0x30: nop                 # hazard break
    "hFC071EE3".U(32.W), // 0x34: bne x14,x0,-36     # inner loop → 0x10
    "hFFF60613".U(32.W), // 0x38: addi x12,x12,-1    # outer--
    "h00000013".U(32.W), // 0x3C: nop                 # hazard break
    "hFC0614E3".U(32.W), // 0x40: bne x12,x0,-56     # outer loop → 0x08
    "h0005A503".U(32.W), // 0x44: lw x10,0(x11)      # x10=arr[0]=1
    "h00000013".U(32.W)  // 0x48: nop
  ))

  // ---------------------------------------------------------------
  // BENCHMARK 3: Histogram (32 inputs, 8 bins)
  // inputs [0..7]x4 at 0x080, bins at 0x100, expected x10=bins[0]=4
  // ---------------------------------------------------------------
  val bench3 = VecInit(Seq(
    "h08000613".U(32.W), // 0x00: addi x12,x0,128    # input ptr=0x80
    "h10000693".U(32.W), // 0x04: addi x13,x0,256    # end ptr=0x100
    "h10000713".U(32.W), // 0x08: addi x14,x0,256    # bin base=0x100
    "h00062783".U(32.W), // 0x0C: lw x15,0(x12)      # v=input[i]  LOOP
    "h00F787B3".U(32.W), // 0x10: add x16,x15,x15    # x16=v*2
    "h01080833".U(32.W), // 0x14: add x16,x16,x16    # x16=v*4
    "h01070993".U(32.W), // 0x18: add x19,x14,x16    # x19=&bins[v]
    "h0009A903".U(32.W), // 0x1C: lw x18,0(x19)      # x18=bins[v]
    "h00190913".U(32.W), // 0x20: addi x18,x18,1     # bins[v]++
    "h0129A023".U(32.W), // 0x24: sw x18,0(x19)      # store back
    "h00460613".U(32.W), // 0x28: addi x12,x12,4     # ptr++
    "hFE0694E3".U(32.W), // 0x2C: bne x12,x13,-24    # loop
    "h00072503".U(32.W), // 0x30: lw x10,0(x14)      # x10=bins[0]
    "h00000013".U(32.W), // 0x34: nop
    "h00000013".U(32.W)  // 0x38: nop
  ))

  // ---------------------------------------------------------------
  // BENCHMARK 4: Write-Through Test
  // Load arr[0]=8, arr[1]=7 (cold misses → fill cache), then
  // SW 99→arr[0] and 42→arr[1] (write hits).
  // Read arr[0] into x10 → expect 99.
  // KEY CHECK: DataRAM arr[0]=99, arr[1]=42 confirms write-through.
  // ---------------------------------------------------------------
  val bench4 = VecInit(Seq(
    "h08000513".U(32.W), // 0x00: addi x10,x0,128    # base = 0x80
    "h00052583".U(32.W), // 0x04: lw   x11,0(x10)    # x11=arr[0]=8  (cold miss)
    "h00452603".U(32.W), // 0x08: lw   x12,4(x10)    # x12=arr[1]=7  (cold miss)
    "h06300693".U(32.W), // 0x0C: addi x13,x0,99     # sentinel A
    "h00D52023".U(32.W), // 0x10: sw   x13,0(x10)    # WRITE HIT: arr[0]=99
    "h02A00713".U(32.W), // 0x14: addi x14,x0,42     # sentinel B
    "h00E52223".U(32.W), // 0x18: sw   x14,4(x10)    # WRITE HIT: arr[1]=42
    "h00052503".U(32.W), // 0x1C: lw   x10,0(x10)    # x10=arr[0] from cache → 99
    "h00000013".U(32.W), // 0x20: nop
    "h00000013".U(32.W)  // 0x24: nop
  ))


  // ---------------------------------------------------------------
  // BENCHMARK 5: Tier 1 ISA Test
  // Tests: XOR, OR, AND, SLL, SRL, SRA, SUB, SLTU, SLLI, SRLI
  // Chain of dependent operations — x10=16 only if all ops correct
  // ---------------------------------------------------------------
  val bench5 = VecInit(Seq(
    "h00F00593".U(32.W), // 0x00: addi x11,x0,15       # x11=15 (0b1111)
    "h00200613".U(32.W), // 0x04: addi x12,x0,2        # x12=2
    "h00600793".U(32.W), // 0x08: addi x15,x0,6        # x15=6  (0b0110)
    "h00F5C6B3".U(32.W), // 0x0C: xor  x13,x11,x15    # 15^6=9
    "h00F6E6B3".U(32.W), // 0x10: or   x13,x13,x15    # 9|6=15
    "h00B6F6B3".U(32.W), // 0x14: and  x13,x13,x11    # 15&15=15
    "h00C696B3".U(32.W), // 0x18: sll  x13,x13,x12    # 15<<2=60
    "h00C6D6B3".U(32.W), // 0x1C: srl  x13,x13,x12    # 60>>2=15
    "hFC400713".U(32.W), // 0x20: addi x14,x0,-60      # x14=-60
    "h40C75733".U(32.W), // 0x24: sra  x14,x14,x12    # -60>>2=-15
    "h40E686B3".U(32.W), // 0x28: sub  x13,x13,x14    # 15-(-15)=30
    "h00B7B833".U(32.W), // 0x2C: sltu x16,x15,x11    # 6<15=1
    "h00169693".U(32.W), // 0x30: slli x13,x13,1      # 30<<1=60
    "h0026D693".U(32.W), // 0x34: srli x13,x13,2      # 60>>2=15
    "h01068533".U(32.W), // 0x38: add  x10,x13,x16    # x10=15+1=16
    "h00000013".U(32.W), // 0x3C: nop
    "h00000013".U(32.W)  // 0x40: nop
  ))

  // ---------------------------------------------------------------
  // BENCHMARK 6: Tier 2 ISA Test
  // Tests: LUI, AUIPC, BLT, BGE, BLTU, BGEU
  // Expected x10 = 4106 = 4096(LUI) + 1(BLT) + 2(BGE) + 3(BLTU) + 4(BGEU)
  // Each branch skips x_=99 and lands on x_=N, confirming branch taken
  // ---------------------------------------------------------------
  val bench6 = VecInit(Seq(
    "h000015B7".U(32.W), // 0x00: lui   x11,1          # x11=4096
    "h00000617".U(32.W), // 0x04: auipc x12,0          # x12=PC=4
    "h00500693".U(32.W), // 0x08: addi  x13,x0,5
    "h00A00713".U(32.W), // 0x0C: addi  x14,x0,10
    "h00E6C463".U(32.W), // 0x10: blt   x13,x14,+8     # 5<10 → skip 99
    "h06300793".U(32.W), // 0x14: addi  x15,x0,99      # SKIPPED
    "h00100793".U(32.W), // 0x18: addi  x15,x0,1       # x15=1
    "h00D75463".U(32.W), // 0x1C: bge   x14,x13,+8     # 10>=5 → skip 99
    "h06300813".U(32.W), // 0x20: addi  x16,x0,99      # SKIPPED
    "h00200813".U(32.W), // 0x24: addi  x16,x0,2       # x16=2
    "h00E6E463".U(32.W), // 0x28: bltu  x13,x14,+8     # 5<10u → skip 99
    "h06300893".U(32.W), // 0x2C: addi  x17,x0,99      # SKIPPED
    "h00300893".U(32.W), // 0x30: addi  x17,x0,3       # x17=3
    "h00D77463".U(32.W), // 0x34: bgeu  x14,x13,+8     # 10>=5u → skip 99
    "h06300913".U(32.W), // 0x38: addi  x18,x0,99      # SKIPPED
    "h00400913".U(32.W), // 0x3C: addi  x18,x0,4       # x18=4
    "h00F58533".U(32.W), // 0x40: add   x10,x11,x15
    "h01050533".U(32.W), // 0x44: add   x10,x10,x16
    "h01150533".U(32.W), // 0x48: add   x10,x10,x17
    "h01250533".U(32.W), // 0x4C: add   x10,x10,x18
    "h00000013".U(32.W), // 0x50: nop
    "h00000013".U(32.W)  // 0x54: nop
  ))

  // ---------------------------------------------------------------
  // BENCHMARK 7: Tier 3 JAL/JALR Test
  // Tests function call and return:
  //   main calls func(5) via JAL, func returns 15 via JALR
  //   after return: x10 += 100 → x10 = 115
  // Expected: x10=115, x1(ra)=12(0x0C)
  // ---------------------------------------------------------------
  val bench7 = VecInit(Seq(
    "h00000513".U(32.W), // 0x00: addi x10,x0,0      # x10=0
    "h00500593".U(32.W), // 0x04: addi x11,x0,5      # arg x11=5
    "h010000EF".U(32.W), // 0x08: jal  x1,+16        # call func@0x18, ra=0x0C
    "h06450513".U(32.W), // 0x0C: addi x10,x10,100  # x10 += 100 after return
    "h00000013".U(32.W), // 0x10: nop
    "h00000013".U(32.W), // 0x14: nop
    "h00058513".U(32.W), // 0x18: addi x10,x11,0     # func: x10=arg=5
    "h00A50513".U(32.W), // 0x1C: addi x10,x10,10    # x10=15
    "h00008067".U(32.W), // 0x20: jalr x0,x1,0       # return to ra(0x0C)
    "h00000013".U(32.W)  // 0x24: nop
  ))


  // ---------------------------------------------------------------
  // BENCHMARK 8: Tier 3 Part 2 — Byte/Halfword Memory Ops
  // Tests: SW, LB, LBU, LH, LHU, SB, SH, LW
  // x15=0xAABB11DD (after SB), x16=0x005511DD (after SH)
  // x10=39523 (checksum: LBU+LB+LH+LHU)
  // ---------------------------------------------------------------
  val bench8 = VecInit(Seq(
    "h08000A13".U(32.W), // 0x00: addi x20,x0,0x80
    "hAABBDAB7".U(32.W), // 0x04: lui  x21,0xAABBD
    "hCDDA8A93".U(32.W), // 0x08: addi x21,x21,-0x323  # x21=0xAABBCCDD
    "h015A2023".U(32.W), // 0x0C: sw   x21,0(x20)
    "h00000013".U(32.W), // 0x10: nop
    "h00000013".U(32.W), // 0x14: nop
    "h000A0503".U(32.W), // 0x18: lb   x10,0(x20)  # -35
    "h00000013".U(32.W), // 0x1C: nop
    "h00000013".U(32.W), // 0x20: nop
    "h000A4583".U(32.W), // 0x24: lbu  x11,0(x20)  # 221
    "h00000013".U(32.W), // 0x28: nop
    "h00000013".U(32.W), // 0x2C: nop
    "h001A0603".U(32.W), // 0x30: lb   x12,1(x20)  # -52
    "h00000013".U(32.W), // 0x34: nop
    "h00000013".U(32.W), // 0x38: nop
    "h000A1683".U(32.W), // 0x3C: lh   x13,0(x20)  # -13091
    "h00000013".U(32.W), // 0x40: nop
    "h00000013".U(32.W), // 0x44: nop
    "h000A5703".U(32.W), // 0x48: lhu  x14,0(x20)  # 52445
    "h00000013".U(32.W), // 0x4C: nop
    "h00000013".U(32.W), // 0x50: nop
    "h01100B13".U(32.W), // 0x54: addi x22,x0,0x11
    "h016A00A3".U(32.W), // 0x58: sb   x22,1(x20)  # byte1=0x11 → 0xAABB11DD
    "h00000013".U(32.W), // 0x5C: nop
    "h00000013".U(32.W), // 0x60: nop
    "h000A2783".U(32.W), // 0x64: lw   x15,0(x20)  # expect 0xAABB11DD
    "h00000013".U(32.W), // 0x68: nop
    "h00000013".U(32.W), // 0x6C: nop
    "h05500B93".U(32.W), // 0x70: addi x23,x0,0x55
    "h017A1123".U(32.W), // 0x74: sh   x23,2(x20)  # half at offset 2 → 0x005511DD
    "h00000013".U(32.W), // 0x78: nop
    "h00000013".U(32.W), // 0x7C: nop
    "h000A2803".U(32.W), // 0x80: lw   x16,0(x20)  # expect 0x005511DD
    "h00000013".U(32.W), // 0x84: nop
    "h00000013".U(32.W), // 0x88: nop
    "h00C58533".U(32.W), // 0x8C: add  x10,x11,x12  # 221+(-52)=169
    "h00D50533".U(32.W), // 0x90: add  x10,x10,x13  # 169+(-13091)=-12922
    "h00E50533".U(32.W), // 0x94: add  x10,x10,x14  # -12922+52445=39523
    "h00000013".U(32.W), // 0x98: nop
    "h00000013".U(32.W)  // 0x9C: nop
  ))
  val rom = c.benchmark match {
    case 0 => bench0
    case 1 => bench1
    case 2 => bench2
    case 3 => bench3
    case 4 => bench4
    case 5 => bench5
    case 6 => bench6
    case 7 => bench7
    case 8 => bench8
    case _ => bench0
  }

  // Hex-file ROM for compiled C benchmarks (bench >= 9)
  // 4096 words = 16KB — enough for Dhrystone (~4KB) and Coremark (~16KB)
  val hex_rom = SyncReadMem(32768, UInt(32.W))
  if (c.benchmark >= 9 && c.hexFile.nonEmpty) {
    loadMemoryFromFileInline(hex_rom, c.hexFile)
  }

  val word_addr = (io.pc >> 2)

  io.inst_out := Mux(
    c.benchmark.U >= 9.U,
    hex_rom(word_addr(11, 0)),
    Mux(
      word_addr < rom.length.U,
      rom(word_addr(log2Ceil(rom.length) - 1, 0)),
      0.U
    )
  )
}