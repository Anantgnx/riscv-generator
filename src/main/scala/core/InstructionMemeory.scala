package core

import chisel3._
import chisel3.util._

// Single-cycle combinational instruction ROM.
// Switch benchmarks by changing c.benchmark in Config.
//
// Benchmark 0: BNE+SLT validation (sum 1+2+3+4, then SLT)
// Benchmark 1: Vector Reduction   (sum array A[i]+B[i], 16 elements)
// Benchmark 2: Bubble Sort        (sort 8 elements using SLT+BNE)
// Benchmark 3: Histogram          (8 bins, 32 inputs)
//
// Memory layout for benchmarks 1-3:
//   0x000-0x0FF : program (instruction ROM, this file)
//   0x080-0x... : data arrays (DataRAM, initialized separately)
//
// Register convention:
//   x10 = result / accumulator (checked by testbench)
//   x11 = temp
//   x12 = counter / pointer A
//   x13 = pointer B / end pointer
//   x14 = base address
//   x15, x16 = loaded values

class InstructionROM(c: Config) extends Module {
  val io = IO(new Bundle {
    val pc       = Input(UInt(c.xLen.W))
    val inst_out = Output(UInt(c.xLen.W))
  })

  // ---------------------------------------------------------------
  // BENCHMARK 0: 2x2 Matrix Multiply  C = A * B
  // A=[[1,2],[3,4]], B=[[5,6],[7,8]]
  // C=[[19,22],[43,50]]  x10=C[1][1]=50
  // A at 0x080, B at 0x090, C at 0x0A0 (DataRAM words 32-43)
  // ROM expanded to 32 entries (0x00-0x7C)
  // ---------------------------------------------------------------
  val bench0 = VecInit(Seq(
    "h00000593".U(32.W), // 0x00: addi x11,x0,0     # i_off=0
    "h00800713".U(32.W), // 0x04: addi x14,x0,8     # limit=8
    "h08000793".U(32.W), // 0x08: addi x15,x0,128   # base_A=0x80
    "h09000813".U(32.W), // 0x0C: addi x16,x0,144   # base_B=0x90
    "h0A000893".U(32.W), // 0x10: addi x17,x0,160   # base_C=0xA0
    "h00000613".U(32.W), // 0x14: addi x12,x0,0     # j_off=0  LOOP_I
    "h00000913".U(32.W), // 0x18: addi x18,x0,0     # acc=0    LOOP_J
    "h00000693".U(32.W), // 0x1C: addi x13,x0,0     # k_off=0
    "h00B78AB3".U(32.W), // 0x20: add x21,x15,x11   # LOOP_K: addr=bA+i_off
    "h00DA8AB3".U(32.W), // 0x24: add x21,x21,x13   # addr+=k_off
    "h000AA983".U(32.W), // 0x28: lw x19,0(x21)     # A[i][k]
    "h00D68B33".U(32.W), // 0x2C: add x22,x13,x13   # k_off*2
    "h01680AB3".U(32.W), // 0x30: add x21,x16,x22   # addr=bB+k_off*2
    "h00CA8AB3".U(32.W), // 0x34: add x21,x21,x12   # addr+=j_off
    "h000AAA03".U(32.W), // 0x38: lw x20,0(x21)     # B[k][j]
    "h034989B3".U(32.W), // 0x3C: mul x19,x19,x20   # A[i][k]*B[k][j]
    "h01390933".U(32.W), // 0x40: add x18,x18,x19   # acc+=product
    "h00468693".U(32.W), // 0x44: addi x13,x13,4    # k_off+=4
    "h00E6ABB3".U(32.W), // 0x48: slt x23,x13,x14   # k_off<8?
    "hFC0B9AE3".U(32.W), // 0x4C: bne x23,x0,-44    # goto LOOP_K
    "h00B88AB3".U(32.W), // 0x50: add x21,x17,x11   # addr=bC+i_off
    "h00CA8AB3".U(32.W), // 0x54: add x21,x21,x12   # addr+=j_off
    "h012AA023".U(32.W), // 0x58: sw x18,0(x21)     # C[i][j]=acc
    "h00460613".U(32.W), // 0x5C: addi x12,x12,4    # j_off+=4
    "h00E62BB3".U(32.W), // 0x60: slt x23,x12,x14   # j_off<8?
    "hFA0B9AE3".U(32.W), // 0x64: bne x23,x0,-76    # goto LOOP_J
    "h00858593".U(32.W), // 0x68: addi x11,x11,8    # i_off+=8
    "h01000C13".U(32.W), // 0x6C: addi x24,x0,16    # limit=16
    "h0185ABB3".U(32.W), // 0x70: slt x23,x11,x24   # i_off<16?
    "hFA0B90E3".U(32.W), // 0x74: bne x23,x0,-96    # goto LOOP_I
    "h00C8A503".U(32.W), // 0x78: lw x10,12(x17)    # x10=C[1][1]=50
    "h00000013".U(32.W)  // 0x7C: nop               (32 total)
  ))

  val bench1 = VecInit(Seq(
    // Vector Reduction: x10 = sum(C[i]) for i=0..15, C[i]=17, expected=272
    // 16 entries = power of 2, no index truncation issues
    "h00000513".U(32.W), // 0x00: addi x10, x0,  0    (acc = 0)
    "h08000613".U(32.W), // 0x04: addi x12, x0, 128   (ptr = 0x80)
    "h0C000713".U(32.W), // 0x08: addi x14, x0, 192   (end = 0xC0)
    "h00062783".U(32.W), // 0x0c: lw   x15, 0(x12)    LOOP START
    "h00F50533".U(32.W), // 0x10: add  x10, x10, x15
    "h00460613".U(32.W), // 0x14: addi x12, x12, 4
    "hFEE61AE3".U(32.W), // 0x18: bne  x12, x14, -12
    "h00000000".U(32.W), // 0x1c: nop
    "h00000000".U(32.W), // 0x20: nop
    "h00000000".U(32.W), // 0x24: pad
    "h00000000".U(32.W), // 0x28: pad
    "h00000000".U(32.W), // 0x2c: pad
    "h00000000".U(32.W), // 0x30: pad
    "h00000000".U(32.W), // 0x34: pad
    "h00000000".U(32.W), // 0x38: pad
    "h00000000".U(32.W)  // 0x3c: pad  (16 total)
  ))

  // ---------------------------------------------------------------
  // BENCHMARK 2: Bubble Sort (8 elements)
  // Array at 0x080, initialized [8,7,6,5,4,3,2,1] (worst case)
  // Expected: x10 = arr[0] = 1 (sorted ascending)
  //
  // Registers:
  //   x11 = base = 0x80
  //   x12 = outer pass counter (7 down to 0)
  //   x13 = inner ptr
  //   x14 = inner count
  //   x15 = arr[i]
  //   x16 = arr[i+1]
  //   x17 = slt result (swap flag)
  // ---------------------------------------------------------------
  val bench2 = VecInit(Seq(
    "h08000593".U(32.W), // 0x00: addi x11, x0, 128    (base = 0x80)
    "h00700613".U(32.W), // 0x04: addi x12, x0, 7      (outer = 7)
    // OUTER LOOP (0x08):
    "h00058693".U(32.W), // 0x08: addi x13, x11, 0     (ptr = base)
    "h00700713".U(32.W), // 0x0c: addi x14, x0, 7      (inner = 7)
    // INNER LOOP (0x10):
    "h0006A783".U(32.W), // 0x10: lw   x15, 0(x13)     (arr[i])
    "h0046A803".U(32.W), // 0x14: lw   x16, 4(x13)     (arr[i+1])
    "h010828B3".U(32.W), // 0x18: slt  x17, x16, x15   (x17 = arr[i+1] < arr[i])
    "h00088463".U(32.W), // 0x1c: beq  x17, x0, 8      (if no swap, skip)
    "h0106A023".U(32.W), // 0x20: sw   x16, 0(x13)     (arr[i] = arr[i+1])
    "h00F6A223".U(32.W), // 0x24: sw   x15, 4(x13)     (arr[i+1] = arr[i])
    // NO_SWAP (0x28):
    "h00468693".U(32.W), // 0x28: addi x13, x13, 4     (ptr++)
    "hFFF70713".U(32.W), // 0x2c: addi x14, x14, -1    (inner--)
    "hFE071AE3".U(32.W), // 0x30: bne  x14, x0, -12    (inner loop)
    "hFFF60613".U(32.W), // 0x34: addi x12, x12, -1    (outer--)
    "hFD461CE3".U(32.W), // 0x38: bne  x12, x0, -36    (outer loop)
    "h00058503".U(32.W), // 0x3c: lw   x10, 0(x11)     (x10 = arr[0])
    "h00000000".U(32.W), // 0x40: nop (exit)
    "h00000000".U(32.W)  // 0x44: nop
  ))

  // ---------------------------------------------------------------
  // BENCHMARK 3: Histogram (32 inputs, 8 bins)
  // Input array at 0x080 (32 words, values 0-7, uniform distribution)
  // Bins at 0x100 (8 words, initialized to 0)
  // Expected: bins[0..7] = [4,4,4,4,4,4,4,4], x10 = bins[0] = 4
  //
  // Registers:
  //   x12 = input ptr (0x80 → 0x100)
  //   x13 = end ptr = 0x100
  //   x14 = bin base = 0x100
  //   x15 = input value (0-7)
  //   x16 = v*4 (bin byte offset)
  //   x17 = &bins[v]
  //   x18 = bins[v] current count
  // ---------------------------------------------------------------
  val bench3 = VecInit(Seq(
    "h08000613".U(32.W), // 0x00: addi x12, x0, 128    (input ptr = 0x80)
    "h10000693".U(32.W), // 0x04: addi x13, x0, 256    (end ptr = 0x100)
    "h10000713".U(32.W), // 0x08: addi x14, x0, 256    (bin base = 0x100)
    // LOOP (0x0c):
    "h00062783".U(32.W), // 0x0c: lw   x15, 0(x12)     (v = input[i])
    "h00F787B3".U(32.W), // 0x10: add  x16, x15, x15   (v*2)
    "h010808B3".U(32.W), // 0x14: add  x17, x16, x16   (v*4)  -- wait: add x17,x16,x16 = 010 80 8B3? recalc below
    "h01170933".U(32.W), // 0x18: add  x18, x14, x17   (&bins[v] = base + v*4)  -- recalc
    "h00092903".U(32.W), // 0x1c: lw   x18, 0(x18)     (bins[v])  -- wait, x18 is addr then data, need another reg
    // NOTE: using x19 for the address, x18 for the count
    // Reassigning for clarity — see corrected sequence below
    "h00000000".U(32.W), // placeholder
    "h00000000".U(32.W)  // placeholder
  ))
  // Benchmark 3 corrected (using x19 for bin address):
  val bench3_fixed = VecInit(Seq(
    "h08000613".U(32.W), // 0x00: addi x12, x0, 128    (input ptr = 0x80)
    "h10000693".U(32.W), // 0x04: addi x13, x0, 256    (end ptr = 0x100)
    "h10000713".U(32.W), // 0x08: addi x14, x0, 256    (bin base = 0x100)
    // LOOP (0x0c):
    "h00062783".U(32.W), // 0x0c: lw   x15, 0(x12)     (v = input[i])
    "h00F787B3".U(32.W), // 0x10: add  x16, x15, x15   (x16 = v*2)
    "h01080833".U(32.W), // 0x14: add  x16, x16, x16   (x16 = v*4)
    "h01070993".U(32.W), // 0x18: add  x19, x14, x16   (x19 = &bins[v])
    "h0009A903".U(32.W), // 0x1c: lw   x18, 0(x19)     (x18 = bins[v])
    "h00190913".U(32.W), // 0x20: addi x18, x18, 1     (bins[v]++)
    "h0129A023".U(32.W), // 0x24: sw   x18, 0(x19)     (store back)
    "h00460613".U(32.W), // 0x28: addi x12, x12, 4     (input ptr++)
    "hFE0694E3".U(32.W), // 0x2c: bne  x12, x13, -24   (loop)
    "h00072503".U(32.W), // 0x30: lw   x10, 0(x14)     (x10 = bins[0])
    "h00000000".U(32.W), // 0x34: nop (exit)
    "h00000000".U(32.W)  // 0x38: nop
  ))

  // Select benchmark via Config
  val rom = c.benchmark match {
    case 0 => bench0
    case 1 => bench1
    case 2 => bench2
    case 3 => bench3_fixed
    case _ => bench0
  }

  val word_addr = (io.pc >> 2).asUInt
  io.inst_out  := Mux(
    word_addr < rom.length.U,
    rom(word_addr(log2Ceil(rom.length) - 1, 0)),
    0.U
  )
}