package goosea.cpu

import goosea.isa.Imm32_11_0
import goosea.utils.num.*

object csr {
  val MXLEN: U64 = 64
  val CSR_MAX: U64 = 1 << 12
  val VALHEIM_MISA: U64 = (U64(2) << 62) // MXL[1:0]=2 (XLEN is 64)
    | (1 << 20) // Extensions[20] (User mode implemented)
    | (1 << 18) // Extensions[18] (Supervisor mode implemented)
    | (1 << 8) // Extensions[8]  (I Base Instruction Set)
    | (1 << 12) // Extensions[12] (M extension)
    | (1 << 0) // Extensions[0]  (A extension)
    | (1 << 5) // Extensions[5]  (F extension)
    | (1 << 3) // Extensions[3]  (D extension)
    | (1 << 2) // Extensions[2]  (C extension)


  // WARL (Write Any Read Legal) restrictions on CSRs
  // https://github.com/qemu/qemu/blob/master/target/riscv/csr.c
  //val M_MODE_INTERRUPTS: U64 = MSIP_MASK | MTIP_MASK | MEIP_MASK
  //val S_MODE_INTERRUPTS: U64 = SSIP_MASK | STIP_MASK | SEIP_MASK
  //val DELEGABLE_INTERRUPTS: U64 = S_MODE_INTERRUPTS;

  object CSRMap {
    // Unprivileged CSR addresses

    // User floating-point CSRs.
    // Floating-point accrued exceptions.
    val FFLAGS: U16 = 0x001
    // Floating-point dynamic rounding mode.
    val FRM: U16 = 0x002

    // Floating-point control and status register (frm + fflags).
    val FCSR: U16 = 0x003
    // Floating-point inexact
    val FCSR_NX_MASK: U64 = 1 << 0
    // Floating-point underflow
    val FCSR_UF_MASK: U64 = 1 << 1
    // Floating-point overflow
    val FCSR_OF_MASK: U64 = 1 << 2
    // Floating-point divide-by-zero
    val FCSR_DZ_MASK: U64 = 1 << 3
    // Floating-point invalid operation
    val FCSR_NV_MASK: U64 = 1 << 4

    // User Counter/Timers.
    // Cycle counter for RDCYCLE instruction.
    val CYCLE: U16 = 0xC00
    // Timer for RDTIME instruction.
    val TIME: U16 = 0xC01
  }

  type CSRAddr = Imm32_11_0

  final class CSRRegs(csrs: Array[U64] = new Array[U64](CSR_MAX.toInt)) {
    def read(addr: CSRAddr): U64 = this.read_unchecked(addr.decode)

    def read_unchecked(addr: U32): U64 = this.read_unchecked(addr.toInt)

    def read_unchecked(addr: Int): U64 = addr match {
      // TODO: add special cases
      case addr => csrs(addr)
    }

    def write_unchecked(addr: Int, value: U64): Unit = addr match {
      // TODO: add special cases
      case addr => csrs(addr) = value
    }
  }

}
