package goosea.cpu

import goosea.isa.compressed.*
import goosea.isa.untyped.*
import goosea.utils.num.*
import goosea.mem.*
import goosea.cpu.bus.*
import goosea.cpu.csr.CSRMap.{FCSR, FCSR_DZ_MASK}
import goosea.cpu.csr.CSRRegs
import goosea.isa.*

import scala.collection.mutable

/* VM modes (satp.mode) privileged ISA V20211203 */
val VM_V20211203_MBARE = 0
val VM_V20211203_SV39 = 8
val VM_V20211203_SV48 = 9
val VM_V20211203_SV57 = 10
val VM_V20211203_SV64 = 11

type VMMode = U8

object VMMode {
  val MBARE: VMMode = VM_V20211203_MBARE
  val SV39: VMMode = VM_V20211203_SV39
  val SV48: VMMode = VM_V20211203_SV48
  val SV57: VMMode = VM_V20211203_SV57
  val SV64: VMMode = VM_V20211203_SV64
}

def sext_w32(x: U32): U64 = {
  val i: Int = x.toInt
  val l: Long = i
  U64(l)
}
def sext_w32(x: U64): U64 = {
  val i: Int = x.toInt
  val l: Long = i
  U64(l)
}
def sext_w32(x: Int):U64 = {
  val l: Long = x
  U64(l)
}

def sext_w8(x: U8): U64 = {
  val i: Byte = x.toByte
  val l: Long = i
  U64(l)
}

def sext_w16(x: U16): U64 = {
  val i: Short = x.toShort
  val l: Long = i
  U64(l)
}
def zext_d8(x: U8): U64 = x.toU64

def zext_d16(x: U16): U64 = x.toU64
def zext_d32(x: U32): U64 = x.toU64

sealed trait PrivilegeMode

object PrivilegeMode {
  case object User extends PrivilegeMode

  case object Supervisor extends PrivilegeMode

  case object Machine extends PrivilegeMode
}

final class RV64CPU(
                     regs: Regs = Regs(),
                     csrs: CSRRegs = CSRRegs(),
                     mode: PrivilegeMode = PrivilegeMode.Machine,
                     bus: Bus = Bus(),
                     // used by LR/SC
                     reserved: mutable.Set[U64] = mutable.Set(),
                     journal: Journal = JournalDisabled,
                     // used by wfi instruction
                     wfi: Boolean = false,
                     // Virtual memory translation mode
                     vmmode: VMMode = VMMode.MBARE,
                     // physical page number used in virtual memory translation
                     vmppn: U64 = 0,
                   ) {
  def fetchMem(addr: U64): U32 = {
    val paddr = this.translate(addr, Reason.Fetch)
    bus.read32(paddr)
  }

  def readMem8(addr: U64): U8 = {
    val paddr = this.translate(addr, Reason.Read)
    val data = bus.read8(paddr)
    journal.trace(Trace.TraceMem.Read(addr, paddr, 1, data.toString))
    data
  }

  def readMem16(addr: U64): U16 = {
    val paddr = this.translate(addr, Reason.Read)
    val data = bus.read16(paddr)
    journal.trace(Trace.TraceMem.Read(addr, paddr, 2, data.toString))
    data
  }

  def readMem32(addr: U64): U32 = {
    val paddr = this.translate(addr, Reason.Read)
    val data = bus.read32(paddr)
    journal.trace(Trace.TraceMem.Read(addr, paddr, 4, data.toString))
    data
  }

  def readMem64(addr: U64): U64 = {
    val paddr = this.translate(addr, Reason.Read)
    val data = bus.read64(paddr)
    journal.trace(Trace.TraceMem.Read(addr, paddr, 8, data.toString))
    data
  }

  def writeMem8(addr: U64, data: U8) = {
    val paddr = this.translate(addr, Reason.Write)
    bus.write8(paddr, data)
    journal.trace(Trace.TraceMem.Write(addr, paddr, 1, data.toString))
  }

  def writeMem16(addr: U64, data: U16) = {
    val paddr = this.translate(addr, Reason.Write)
    bus.write16(paddr, data)
    journal.trace(Trace.TraceMem.Write(addr, paddr, 2, data.toString))
  }

  def writeMem32(addr: U64, data: U32) = {
    val paddr = this.translate(addr, Reason.Write)
    bus.write32(paddr, data)
    journal.trace(Trace.TraceMem.Write(addr, paddr, 4, data.toString))
  }

  def writeMem64(addr: U64, data: U64) = {
    val paddr = this.translate(addr, Reason.Write)
    bus.write64(paddr, data)
    journal.trace(Trace.TraceMem.Write(addr, paddr, 8, data.toString))
  }

  def readReg(reg: Reg): U64 = {
    val x = regs.read(reg)
    journal.trace(Trace.TraceReg.Read(reg, x))
    x
  }

  def writeReg(reg: Reg, value: U64): Unit = {
    regs.write(reg, value)
    journal.trace(Trace.TraceReg.Write(reg, value))
  }

  def ldst_addr(rs1: Reg.X, offset: Imm32): U64 = regs.read(rs1) + sext_w32(offset.decodeSext)

  def amo_addr(rs1: Reg.X, align: Int): U64 = {
    val addr = regs.read(rs1)
    if(addr % align != 0) {
      throw CPUThrowable.LoadAddressMisaligned(addr)
    }
    addr
  }


  def readPC: U64 = readReg(Reg.PC)

  def writePC(pc: U64) = writeReg(Reg.PC, pc)

  // TODO

  final case class Fetch(pc: U64, bytecode: Bytecode, compressed: Bytecode16)

  def fetch: Fetch = {
    val pc = readPC
    ???
  }

  def fetchForMock(pc: U64): U32 = {
    ???
  }

  def mockFetch(pc: U64, instr: U32): Fetch = {
    ???
  }

  final case class Decode(from: Either[Bytecode, Bytecode16], decoded: Instr)

  def decode(fetch: Fetch): Decode = {
    ???
  }

  // throws CPUThrowable
  def execute(pc: U64, instr: Instr, isCompressed: Boolean): Unit = {
    var nextPC = if (isCompressed) pc + 2 else pc + 4
    journal.trace(Trace.TraceInstr.PrepareExecute(pc, instr))
    instr match {
      case NOP => {}
      // nop is also encoded as `ADDI x0, x0, 0`
      case RV32Instr.ADDI(Reg.X(0), Reg.X(0), Imm32(0)) => {}
      case RV32Instr.LUI(rd, imm) => regs.write(rd, sext_w32(imm.decode))
      case RV32Instr.AUIPC(rd, offest) => regs.write(rd, pc + sext_w32(offest.decode))
      case RV32Instr.JAL(rd, imm) => {
        val offset = sext_w32(imm.decodeSext)
        val target = pc + offset
        regs.write(rd, nextPC)
        nextPC = target
      }
      case RV32Instr.JALR(rd, rs1, imm) => {
        val offset = sext_w32(imm.decodeSext)
        val target = ((regs.read(rs1) + offset) >> 1) << 1
        regs.write(rd, nextPC)
        nextPC = target
      }
      case RV32Instr.BEQ(rs1, rs2, imm) => {
        if (regs.read(rs1) == regs.read(rs2)) {
          nextPC = pc + sext_w32(imm.decodeSext)
        }
      }
      case RV32Instr.BNE(rs1, rs2, imm) => {
        if (regs.read(rs1) != regs.read(rs2)) {
          nextPC = pc + sext_w32(imm.decodeSext)
        }
      }
      case RV32Instr.BLT(rs1, rs2, imm) => {
        if (regs.read(rs1).toLong < regs.read(rs2).toLong) {
          nextPC = pc + sext_w32(imm.decodeSext)
        }
      }
      case RV32Instr.BGE(rs1, rs2, imm) => {
        if (regs.read(rs1).toLong >= regs.read(rs2).toLong) {
          nextPC = pc + sext_w32(imm.decodeSext)
        }
      }
      case RV32Instr.BLTU(rs1, rs2, imm) => {
        if (regs.read(rs1) < regs.read(rs2)) {
          nextPC = pc + sext_w32(imm.decodeSext)
        }
      }
      case RV32Instr.BGEU(rs1, rs2, imm) => {
        if (regs.read(rs1) >= regs.read(rs2)) {
          nextPC = pc + sext_w32(imm.decodeSext)
        }
      }
      case RV32Instr.LB(rd, rs1, offset) => {
        val addr = ldst_addr(rs1, offset)
        val data = readMem8(addr)
        regs.write(rd, sext_w8(data))
      }
      case RV32Instr.LH(rd, rs1, offset) => {
        val addr = ldst_addr(rs1, offset)
        val data = readMem16(addr)
        regs.write(rd, sext_w16(data))
      }
      case RV32Instr.LW(rd, rs1, offset) => {
        val addr = ldst_addr(rs1, offset)
        val data = readMem32(addr)
        regs.write(rd, sext_w32(data))
      }
      case RV64Instr.LD(rd, rs1, offset) => {
        val addr = ldst_addr(rs1, offset)
        val data = readMem64(addr)
        regs.write(rd, data)
      }
      case RV32Instr.LBU(rd, rs1, offset) => {
        val addr = ldst_addr(rs1, offset)
        val data = readMem8(addr)
        regs.write(rd, zext_d8(data))
      }
      case RV32Instr.LHU(rd, rs1, offset) => {
        val addr = ldst_addr(rs1, offset)
        val data = readMem16(addr)
        regs.write(rd, zext_d16(data))
      }
      case RV64Instr.LWU(rd, rs1, offset) => {
        val addr = ldst_addr(rs1, offset)
        val data = readMem32(addr)
        regs.write(rd, zext_d32(data))
      }
      case RV32Instr.SB(rs1, rs2, offset) => {
        val addr = ldst_addr(rs1, offset)
        val data = regs.read(rs2)
        writeMem8(addr, data.toU8)
      }
      case RV32Instr.SH(rs1, rs2, offset) => {
        val addr = ldst_addr(rs1, offset)
        val data = regs.read(rs2)
        writeMem16(addr, data.toU16)
      }
      case RV32Instr.SW(rs1, rs2, offset) => {
        val addr = ldst_addr(rs1, offset)
        val data = regs.read(rs2)
        writeMem32(addr, data.toU32)
      }
      case RV64Instr.SD(rs1, rs2, offset) => {
        val addr = ldst_addr(rs1, offset)
        val data = regs.read(rs2)
        writeMem64(addr, data)
      }
      case RV32Instr.ADDI(rd, rs1, imm) => {
        regs.write(rd, regs.read(rs1) + sext_w32(imm.decodeSext))
      }
      case RV64Instr.ADDIW(rd, rs1, imm) => {
        regs.write(rd, sext_w32(regs.read(rs1).toInt + imm.decodeSext))
      }
      case RV32Instr.XORI(rd, rs1, imm) => {
        regs.write(rd, regs.read(rs1) ^ sext_w32(imm.decodeSext))
      }
      case RV32Instr.ORI(rd, rs1, imm) => {
        regs.write(rd, regs.read(rs1) | sext_w32(imm.decodeSext))
      }
      case RV32Instr.ANDI(rd, rs1, imm) => {
        regs.write(rd, regs.read(rs1) & sext_w32(imm.decodeSext))
      }
      case RV32Instr.SLTI(rd, rs1, shamt) => {
        regs.write(rd, if (regs.read(rs1).toLong < sext_w32(shamt.decodeSext).toLong) 1 else 0)
      }
      case RV32Instr.SLTIU(rd, rs1, shamt) => {
        regs.write(rd, if (regs.read(rs1) < sext_w32(shamt.decodeSext)) 1 else 0)
      }
      case RV32Instr.SLLI(rd, rs1, shamt) => {
        regs.write(rd, regs.read(rs1) << shamt)
      }
      case RV32Instr.SRLI(rd, rs1, shamt) => {
        regs.write(rd, regs.read(rs1) >> shamt)
      }
      case RV64Instr.SLLI(rd, rs1, shamt) => {
        regs.write(rd, regs.read(rs1) << shamt)
      }
      case RV64Instr.SRLI(rd, rs1, shamt) => {
        regs.write(rd, regs.read(rs1) >> shamt)
      }
      case RV32Instr.SRAI(rd, rs1, shamt) => {
        regs.write(rd, U64(regs.read(rs1).toLong >> shamt.toInt))
      }
      case RV64Instr.SRAI(rd, rs1, shamt) => {
        regs.write(rd, U64(regs.read(rs1).toLong >> shamt.toInt))
      }
      case RV64Instr.SLLIW(rd, rs1, shamt) => {
        regs.write(rd, sext_w32(regs.read(rs1).toU32 << shamt))
      }
      case RV64Instr.SRLIW(rd, rs1, shamt) => {
        regs.write(rd, sext_w32(regs.read(rs1).toU32 >> shamt))
      }
      case RV64Instr.SRAIW(rd, rs1, shamt) => {
        regs.write(rd, sext_w32(U32(regs.read(rs1).toInt >> shamt.toInt)))
      }
      case RV32Instr.ADD(rd, rs1, rs2) => {
        regs.write(rd, regs.read(rs1) + regs.read(rs2))
      }
      case RV32Instr.SUB(rd, rs1, rs2) => {
        regs.write(rd, regs.read(rs1) - regs.read(rs2))
      }
      case RV64Instr.ADDW(rd, rs1, rs2) => {
        regs.write(rd, sext_w32(regs.read(rs1) + regs.read(rs2)))
      }
      case RV64Instr.SUBW(rd, rs1, rs2) => {
        regs.write(rd, sext_w32(regs.read(rs1) - regs.read(rs2)))
      }
      case RV32Instr.SLT(rd, rs1, rs2) => {
        regs.write(rd, if (regs.read(rs1).toLong < regs.read(rs2).toLong) 1 else 0)
      }
      case RV32Instr.SLTU(rd, rs1, rs2) => {
        regs.write(rd, if (regs.read(rs1) < regs.read(rs2)) 1 else 0)
      }
      case RV32Instr.SLL(rd, rs1, rs2) => {
        regs.write(rd, regs.read(rs1) << (regs.read(rs2) & 63))
      }
      case RV32Instr.SRL(rd, rs1, rs2) => {
        regs.write(rd, regs.read(rs1) >> (regs.read(rs2) & 63))
      }
      case RV64Instr.SLLW(rd, rs1, rs2) => {
        regs.write(rd, sext_w32(regs.read(rs1).toU32 << (regs.read(rs2) & 31)))
      }
      case RV64Instr.SRLW(rd, rs1, rs2) => {
        regs.write(rd, sext_w32(regs.read(rs1).toU32 >> (regs.read(rs2) & 31)))
      }
      case RV32Instr.SRA(rd, rs1, rs2) => {
        regs.write(rd, U64(regs.read(rs1).toLong >> (regs.read(rs2).toInt & 63)))
      }
      case RV64Instr.SRAW(rd, rs1, rs2) => {
        regs.write(rd, sext_w32(U32(regs.read(rs1).toInt >> (regs.read(rs2).toInt & 31))))
      }

      case RV32Instr.XOR(rd, rs1, rs2) => {
        regs.write(rd, regs.read(rs1) ^ regs.read(rs2))
      }
      case RV32Instr.OR(rd, rs1, rs2) => {
        regs.write(rd, regs.read(rs1) | regs.read(rs2))
      }
      case RV32Instr.AND(rd, rs1, rs2) => {
        regs.write(rd, regs.read(rs1) & regs.read(rs2))
      }

      case RV32Instr.FENCE(_, _, _, _, _) => {}
      case RV64Instr.FENCE_I(_, _, _) => {}
      case RV32Instr.FENCE_TSO => {}
      case RV32Instr.PAUSE => throw new UnsupportedOperationException("not implemented at PC=0x" + pc.toString(16))
      case RV32Instr.ECALL => this.mode match {
        case PrivilegeMode.User => throw CPUThrowable.UserEcall
        case PrivilegeMode.Supervisor => throw CPUThrowable.SupervisorEcall
        case PrivilegeMode.Machine => throw CPUThrowable.MachineEcall
      }

      // RVM
      // TODO: implement
      case RV32Instr.MUL(rd, rs1, rs2) => {
        regs.write(rd, U64(regs.read(rs1).toLong * regs.read(rs2).toLong))
      }
      case RV32Instr.MULH(rd, rs1, rs2) => ???
      case RV32Instr.MULHSU(rd, rs1, rs2) => ???
      case RV32Instr.MULHU(rd, rs1, rs2) => ???
      case RV32Instr.DIV(rd, rs1, rs2) => {
        val dividend = regs.read(rs1).toLong
        val divisor = regs.read(rs2).toLong
        val result = if (divisor == 0) {
          csrs.write_unchecked(FCSR, csrs.read_unchecked(FCSR) | FCSR_DZ_MASK)
          U64.MaxValue
        } else if (dividend == Long.MinValue && divisor == -1) {
          U64(Long.MinValue)
        } else {
          U64(dividend / divisor)
        }
        regs.write(rd, result)
      }
      case RV32Instr.DIVU(rd, rs1, rs2) => {
        val dividend = regs.read(rs1)
        val divisor = regs.read(rs2)
        val result = if (divisor == 0) {
          csrs.write_unchecked(FCSR, csrs.read_unchecked(FCSR) | FCSR_DZ_MASK)
          U64.MaxValue
        } else {
          dividend / divisor
        }
        regs.write(rd, result)
      }
      case RV32Instr.REM(rd, rs1, rs2) => {
        val dividend = regs.read(rs1).toLong
        val divisor = regs.read(rs2).toLong
        val result = if (divisor == 0) {
          U64(dividend)
        } else if (dividend == Long.MinValue && divisor == -1) {
          U64(0)
        } else {
          U64(dividend % divisor)
        }
        regs.write(rd, result)
      }
      case RV32Instr.REMU(rd, rs1, rs2) => {
        val dividend = regs.read(rs1)
        val divisor = regs.read(rs2)
        val result = if (divisor == 0) {
          dividend
        } else {
          dividend % divisor
        }
        regs.write(rd, result)
      }
      case RV64Instr.MULW(rd, rs1, rs2) => {
        regs.write(rd, sext_w32(U32(regs.read(rs1).toInt * regs.read(rs2).toInt)))
      }
      case RV64Instr.DIVW(rd, rs1, rs2) => {
        val dividend = regs.read(rs1).toInt
        val divisor = regs.read(rs2).toInt
        val result = if (divisor == 0) {
          csrs.write_unchecked(FCSR, csrs.read_unchecked(FCSR) | FCSR_DZ_MASK)
          U64.MaxValue
        } else if (dividend == Int.MinValue && divisor == -1) {
          sext_w32(dividend)
        } else {
          sext_w32(dividend / divisor)
        }
        regs.write(rd, result)
      }
      case RV64Instr.DIVUW(rd, rs1, rs2) => {
        val dividend = regs.read(rs1).toU32
        val divisor = regs.read(rs2).toU32
        val result = if (divisor == 0) {
          csrs.write_unchecked(FCSR, csrs.read_unchecked(FCSR) | FCSR_DZ_MASK)
          U64.MaxValue
        } else {
          sext_w32(dividend / divisor)
        }
        regs.write(rd, result)
      }
      case RV64Instr.REMW(rd, rs1, rs2) => {
        val dividend = regs.read(rs1).toInt
        val divisor = regs.read(rs2).toInt
        val result = if (divisor == 0) {
          sext_w32(dividend)
        } else if (dividend == Int.MinValue && divisor == -1) {
          U64(0)
        } else {
          sext_w32(dividend % divisor)
        }
        regs.write(rd, result)
      }
      case RV64Instr.REMUW(rd, rs1, rs2) => {
        val dividend = regs.read(rs1).toU32
        val divisor = regs.read(rs2).toU32
        val result = if (divisor == 0) {
          sext_w32(dividend)
        } else {
          sext_w32(dividend % divisor)
        }
        regs.write(rd, result)
      }

      // the goosea trap
      case RV32Instr.EBREAK => throw CPUThrowable.Breakpoint

      // RVA
      case RV32Instr.LR_W(rd, rs1, _) => {
        val addr = amo_addr(rs1, 4)
        val data = readMem32(addr)
        regs.write(rd, sext_w32(data))
        reserved.addOne(addr)
      }
      case RV32Instr.SC_W(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 4)
        if(reserved.contains(addr)) {
          // "Regardless of success or failure, executing an SC.W instruction
          // invalidates any reservation held by this hart. "
          reserved.remove(addr)
          writeMem32(addr, regs.read(rs2).toU32)
          regs.write(rd, 0)
        } else {
          reserved.remove(addr)
          regs.write(rd, 1)
        }
      }
      case RV64Instr.LR_D(rd, rs1, _) => {
        val addr = amo_addr(rs1, 8)
        val data = readMem64(addr)
        regs.write(rd, data)
        reserved.addOne(addr)
      }
      case RV64Instr.SC_D(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 8)
        if(reserved.contains(addr)) {
          reserved.remove(addr)
          writeMem64(addr, regs.read(rs2))
          regs.write(rd, 0)
        } else {
          reserved.remove(addr)
          regs.write(rd, 1)
        }
      }

      case RV32Instr.AMOADD_W(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 4)
        val data = readMem32(addr)
        writeMem32(addr, data + regs.read(rs2).toU32)
        regs.write(rd, sext_w32(data))
      }
      case RV64Instr.AMOADD_D(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 8)
        val data = readMem64(addr)
        writeMem64(addr, data + regs.read(rs2))
        regs.write(rd, data)
      }
      case RV32Instr.AMOSWAP_W(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 4)
        val data = readMem32(addr)
        writeMem32(addr, regs.read(rs2).toU32)
        regs.write(rd, sext_w32(data))
      }
      case RV64Instr.AMOSWAP_D(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 8)
        val data = readMem64(addr)
        writeMem64(addr, regs.read(rs2))
        regs.write(rd, data)
      }

      case RV32Instr.AMOXOR_W(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 4)
        val data = readMem32(addr)
        writeMem32(addr, data ^ regs.read(rs2).toU32)
        regs.write(rd, sext_w32(data))
      }
      case RV32Instr.AMOAND_W(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 4)
        val data = readMem32(addr)
        writeMem32(addr, data & regs.read(rs2).toU32)
        regs.write(rd, sext_w32(data))
      }
      case RV32Instr.AMOOR_W(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 4)
        val data = readMem32(addr)
        writeMem32(addr, data | regs.read(rs2).toU32)
        regs.write(rd, sext_w32(data))
      }
      case RV64Instr.AMOXOR_D(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 8)
        val data = readMem64(addr)
        writeMem64(addr, data ^ regs.read(rs2))
        regs.write(rd, data)
      }
      case RV64Instr.AMOAND_D(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 8)
        val data = readMem64(addr)
        writeMem64(addr, data & regs.read(rs2))
        regs.write(rd, data)
      }
      case RV64Instr.AMOOR_D(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 8)
        val data = readMem64(addr)
        writeMem64(addr, data | regs.read(rs2))
        regs.write(rd, data)
      }

      case RV32Instr.AMOMIN_W(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 4)
        val data = readMem32(addr)
        val rs2v = regs.read(rs2).toU32
        writeMem32(addr, if(data.toInt < rs2v.toInt) data else rs2v)
        regs.write(rd, sext_w32(data))
      }
      case RV64Instr.AMOMIN_D(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 8)
        val data = readMem64(addr)
        val rs2v = regs.read(rs2)
        writeMem64(addr, if(data.toLong < rs2v.toLong) data else rs2v)
        regs.write(rd, data)
      }
      case RV32Instr.AMOMAX_W(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 4)
        val data = readMem32(addr)
        val rs2v = regs.read(rs2).toU32
        writeMem32(addr, if(data.toInt > rs2v.toInt) data else rs2v)
        regs.write(rd, sext_w32(data))
      }
      case RV64Instr.AMOMAX_D(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 8)
        val data = readMem64(addr)
        val rs2v = regs.read(rs2)
        writeMem64(addr, if(data.toLong > rs2v.toLong) data else rs2v)
        regs.write(rd, data)
      }
      case RV32Instr.AMOMINU_W(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 4)
        val data = readMem32(addr)
        val rs2v = regs.read(rs2).toU32
        writeMem32(addr, if(data < rs2v) data else rs2v)
        regs.write(rd, sext_w32(data))
      }
      case RV64Instr.AMOMINU_D(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 8)
        val data = readMem64(addr)
        val rs2v = regs.read(rs2)
        writeMem64(addr, if(data < rs2v) data else rs2v)
        regs.write(rd, data)
      }
      case RV32Instr.AMOMAXU_W(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 4)
        val data = readMem32(addr)
        val rs2v = regs.read(rs2).toU32
        writeMem32(addr, if(data > rs2v) data else rs2v)
        regs.write(rd, sext_w32(data))
      }
      case RV64Instr.AMOMAXU_D(rd, rs1, rs2, _) => {
        val addr = amo_addr(rs1, 8)
        val data = readMem64(addr)
        val rs2v = regs.read(rs2)
        writeMem64(addr, if(data > rs2v) data else rs2v)
        regs.write(rd, data)
      }



    }
    journal.trace(if(isCompressed)Trace.TraceInstr.ExecutedCompressed(pc, instr) else Trace.TraceInstr.Executed(pc, instr))
    regs.write(Reg.PC, nextPC)
  }

  def tick: Unit = {
    if (this.wfi) {
      return
    }
    val fetch = this.fetch
    val Decode(from, decoded) = this.decode(fetch)
    val isCompressed = from.isRight
    this.execute(fetch.pc, decoded, isCompressed)
  }

  def mockTick(pc: U64, instr: U32): Unit = {
    if (this.wfi) {
      return
    }
    val fetch = this.mockFetch(pc, instr)
    val Decode(from, decoded) = this.decode(fetch)
    val isCompressed = from.isRight
    this.execute(fetch.pc, decoded, isCompressed)
  }

  sealed trait Reason

  object Reason {
    case object Fetch extends Reason

    case object Read extends Reason

    case object Write extends Reason
  }

  def translate(addr: U64, reason: Reason): U64 = {
    if (this.vmmode == VMMode.MBARE) {
      return addr
    }

    // 3.1.6.3 Memory Privilege in mstatus Register
    // The MPRV (Modify PRiVilege) bit modifies the effective privilege mode,
    // i.e., the privilege level at which loads and stores execute.
    // When MPRV=0, loads and stores behave as normal, using the translation and protection
    // mechanisms of the current privilege mode.
    // When MPRV=1, load and store memory addresses are translated and protected,
    // and endianness is applied, as though the current privilege mode were set to MPP.
    // Instruction address-translation and protection are unaffected by the setting of MPRV.
    // MPRV is read-only 0 if U-mode is not supported.
    //val eff_mode = reason match {
    //  case Reason.Fetch => this.mode,
    //}
    ???
  }
}

object RV64CPU {
  def apply(): RV64CPU = new RV64CPU()
}

