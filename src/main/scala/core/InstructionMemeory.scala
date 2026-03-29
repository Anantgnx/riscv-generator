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
  // BENCHMARK 0: BNE + SLT Validation
  // sum = 1+2+3+4 = 10, then x10 = (0 < sum) ? 1 : 0
  // Expected: x10 = 1
  // No data memory needed.
  // ---------------------------------------------------------------
  val bench0 = VecInit(Seq(
    "h00000513".U(32.W), // 0x00: addi x10, x0,  0     (sum = 0)
    "h00400613".U(32.W), // 0x04: addi x12, x0,  4     (counter = 4)
    "h00C50533".U(32.W), // 0x08: add  x10, x10, x12   <- LOOP TOP
    "hFFF60613".U(32.W), // 0x0c: addi x12, x12, -1
    "hFE061CE3".U(32.W), // 0x10: bne  x12, x0,  -8
    "h00A02533".U(32.W), // 0x14: slt  x10, x0,  x10
    "h00000000".U(32.W), // 0x18: nop (exit)
    "h00000000".U(32.W)  // 0x1c: nop
  ))

  // ---------------------------------------------------------------
  // BENCHMARK 1: Vector Reduction
  // x10 = sum(C[i]) for i=0..15, where C[i] = A[i]+B[i] = 17 (precomputed)
  // C at 0x080 (16 words of value 17, initialized in DataRAM)
  // Expected: x10 = 16 * 17 = 272
  //
  // Registers:
  //   x10 = accumulator
  //   x12 = ptr (starts 0x80, ends 0xC0)
  //   x14 = end = 0xC0
  //   x15 = C[i]
  // ---------------------------------------------------------------
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