package goosea.isa

import goosea.utils.num._

import scala.annotation.targetName
import scala.language.implicitConversions

sealed trait Reg

object Reg {
  final case class X(f: Fin32) extends Reg

  object X {
    def unapply(x: X):Some[Int] = Some(x.f.toInt)
  }

  final case class F(f: Fin32) extends Reg

  case object PC extends Reg

  case object CSR extends Reg

  case object FCSR extends Reg
}


sealed trait RoundingMode

object RoundingMode {
  // Round to nearest, ties to even
  case object RNE extends RoundingMode

  // Round towards zero
  case object RTZ extends RoundingMode

  // Round towards -infinity
  case object RDN extends RoundingMode

  // Round towards +infinity
  case object RUP extends RoundingMode

  // Round to nearest, ties to max magnitude
  case object RMM extends RoundingMode

  // In instruction's rm field, select dynamic rounding mode;
  // In Rounding Mode register, reserved.
  case object DYN extends RoundingMode
}

// This is used to represent lazily-decoded immediate value
// which is written as `imm[HIGH_BIT:LOW_BIT]` in the risc-v specification.
class Imm32(val highBit: Int, val lowBit: Int, val i: U32) {

  def validBits = highBit - lowBit + 1

  def decode: U32 = {
    val mask = (1 << this.validBits) - 1
    (this.i & mask) << lowBit
  }

  def decodeSext: U32 = {
    val data = this.decode
    val signBit: U32 = highBit
    (data << (U32(31) - signBit)) >> (U32(31) - signBit)
  }

  def bitor(rhs: Imm32): Imm32 = {
    val lhs = this
    Imm32(lhs.highBit, rhs.lowBit, (lhs.decode | rhs.decode) >> rhs.lowBit)
  }
}

object Imm32 {
  def unapply(i: Imm32):Some[Int] = Some(i.decode.toInt)
}


class Imm32_11_0(i: U32) extends Imm32(11, 0, i)

object Imm32_11_0 {
  def from(x: Imm32): Imm32_11_0 = if (x.highBit == 11 && x.lowBit == 0) Imm32_11_0(x.i) else throw new IllegalArgumentException()
}

class Imm32_12_1(i: U32) extends Imm32(12, 1, i)

object Imm32_12_1 {
  def from(x: Imm32): Imm32_12_1 = if (x.highBit == 12 && x.lowBit == 1) Imm32_12_1(x.i) else throw new IllegalArgumentException()
}

class Imm32_4_0(i: U32) extends Imm32(4, 0, i)

class Imm32_31_12(i: U32) extends Imm32(31, 12, i)

class Imm32_20_1(i: U32) extends Imm32(20, 1, i)

object Imm32_20_1 {
  def from(x: Imm32): Imm32_20_1 = if (x.highBit == 20 && x.lowBit == 1) Imm32_20_1(x.i) else throw new IllegalArgumentException()
}

class Imm32_20_20(i: U32) extends Imm32(20, 20, i)

class Imm32_10_1(i: U32) extends Imm32(10, 1, i)

class Imm32_11_11(i: U32) extends Imm32(11, 11, i)

class Imm32_19_12(i: U32) extends Imm32(11, 11, i)

class Imm32_12_12(i: U32) extends Imm32(12, 12, i)

class Imm32_10_5(i: U32) extends Imm32(10, 5, i)

class Imm32_4_1(i: U32) extends Imm32(4, 1, i)

class Imm32_11_5(i: U32) extends Imm32(11, 5, i)

// Atomic instruction flag: Acquire and Release
final case class AQRL(acquire: Boolean, release: Boolean)

sealed trait Instr

case object NOP extends Instr

sealed trait RV64Instr extends Instr

object RV64Instr {
  // RV64I
  final case class LWU(rd: Reg.X, rs1: Reg.X, i: Imm32_11_0) extends RV64Instr

  final case class LD(rd: Reg, rs1: Reg.X, i: Imm32_11_0) extends RV64Instr

  final case class SD(rd: Reg.X, rs1: Reg.X, i: Imm32_11_0) extends RV64Instr

  final case class SLLI(rd: Reg.X, rs1: Reg.X, shiftAmount: U8) extends RV64Instr

  final case class SRLI(rd: Reg.X, rs1: Reg.X, shiftAmount: U8) extends RV64Instr

  final case class SRAI(rd: Reg.X, rs1: Reg.X, shiftAmount: U8) extends RV64Instr

  final case class ADDIW(rd: Reg.X, rs1: Reg.X, i: Imm32_11_0) extends RV64Instr

  final case class SLLIW(rd: Reg, rs1: Reg, shiftAmount: U8) extends RV64Instr

  final case class SRLIW(rd: Reg, rs1: Reg, shiftAmount: U8) extends RV64Instr

  final case class SRAIW(rd: Reg, rs1: Reg, shiftAmount: U8) extends RV64Instr

  final case class ADDW(rd: Reg.X, rs1: Reg.X, rs2: Reg) extends RV64Instr

  final case class SUBW(rd: Reg.X, rs1: Reg.X, rs2: Reg) extends RV64Instr

  final case class SLLW(rd: Reg, rs1: Reg, rs2: Reg) extends RV64Instr

  final case class SRLW(rd: Reg, rs1: Reg, rs2: Reg) extends RV64Instr

  final case class SRAW(rd: Reg, rs1: Reg, rs2: Reg) extends RV64Instr

  // RV64M
  final case class MULW(rd: Reg.X, rs1: Reg.X, rs2: Reg.X) extends RV64Instr

  final case class DIVW(rd: Reg.X, rs1: Reg.X, rs2: Reg.X) extends RV64Instr

  final case class DIVUW(rd: Reg.X, rs1: Reg.X, rs2: Reg.X) extends RV64Instr

  final case class REMW(rd: Reg.X, rs1: Reg.X, rs2: Reg.X) extends RV64Instr

  final case class REMUW(rd: Reg.X, rs1: Reg.X, rs2: Reg.X) extends RV64Instr

  // RV64A
  final case class LR_D(rd: Reg.X, rs1: Reg.X, flag: AQRL) extends RV64Instr

  final case class SC_D(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV64Instr

  final case class AMOSWAP_D(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV64Instr

  final case class AMOADD_D(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV64Instr

  final case class AMOXOR_D(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV64Instr

  final case class AMOAND_D(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV64Instr

  final case class AMOOR_D(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV64Instr

  final case class AMOMIN_D(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV64Instr

  final case class AMOMAX_D(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV64Instr

  final case class AMOMINU_D(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV64Instr

  final case class AMOMAXU_D(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV64Instr

  // RV64F
  final case class FCVT_L_S(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV64Instr

  final case class FCVT_LU_S(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV64Instr

  final case class FCVT_S_L(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV64Instr

  final case class FCVT_S_LU(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV64Instr

  // RV64D
  final case class FCVT_L_D(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV64Instr

  final case class FCVT_LU_D(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV64Instr

  final case class FMV_X_D(rd: Reg, rs1: Reg) extends RV64Instr

  final case class FCVT_D_L(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV64Instr

  final case class FCVT_D_LU(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV64Instr

  final case class FMV_D_X(rd: Reg, rs1: Reg) extends RV64Instr

  // RV32/64 Zicsr
  final case class CSRRW(rd: Reg, rs1: Reg, csr: Imm32_11_0) extends RV64Instr

  final case class CSRRS(rd: Reg, rs1: Reg, csr: Imm32_11_0) extends RV64Instr

  final case class CSRRC(rd: Reg, rs1: Reg, csr: Imm32_11_0) extends RV64Instr

  final case class CSRRWI(rd: Reg, i: Imm32_4_0, csr: Imm32_11_0) extends RV64Instr

  final case class CSRRSI(rd: Reg, i: Imm32_4_0, csr: Imm32_11_0) extends RV64Instr

  final case class CSRRCI(rd: Reg, i: Imm32_4_0, csr: Imm32_11_0) extends RV64Instr

  // RV32/64 Zifencei
  final case class FENCE_I(rd: Reg, rs1: Reg, i: Imm32_11_0) extends RV64Instr

  // Privileged
  case object SRET extends RV64Instr

  case object MRET extends RV64Instr

  case object WFI extends RV64Instr

  final case class SFENCE_VMA(rs1: Reg, rs2: Reg) extends RV64Instr

  final case class SINVAL_VMA(rs1: Reg, rs2: Reg) extends RV64Instr

  case object SFENCE_W_INVAL extends RV64Instr

  case object SFENCE_INVAL_IR extends RV64Instr
}

sealed trait RV32Instr extends Instr

object RV32Instr {
  // RV32I
  final case class LUI(rd: Reg, i: Imm32_31_12) extends RV32Instr

  final case class AUIPC(rd: Reg, i: Imm32_31_12) extends RV32Instr

  final case class JAL(rd: Reg.X, i: Imm32_20_1) extends RV32Instr

  final case class JALR(rd: Reg.X, rs1: Reg.X, i: Imm32_11_0) extends RV32Instr

  final case class BEQ(rs1: Reg.X, rs2: Reg.X, i: Imm32_12_1) extends RV32Instr

  final case class BNE(rs1: Reg.X, rs2: Reg.X, i: Imm32_12_1) extends RV32Instr

  final case class BLT(rs1: Reg.X, rs2: Reg.X, i: Imm32_12_1) extends RV32Instr

  final case class BGE(rs1: Reg.X, rs2: Reg.X, i: Imm32_12_1) extends RV32Instr

  final case class BLTU(rs1: Reg.X, rs2: Reg.X, i: Imm32_12_1) extends RV32Instr

  final case class BGEU(rs1: Reg.X, rs2: Reg.X, i: Imm32_12_1) extends RV32Instr

  final case class LB(rd: Reg.X, rs1: Reg.X, i: Imm32_11_0) extends RV32Instr

  final case class LH(rd: Reg.X, rs1: Reg.X, i: Imm32_11_0) extends RV32Instr

  final case class LW(rd: Reg.X, rs1: Reg.X, i: Imm32_11_0) extends RV32Instr

  final case class LBU(rd: Reg.X, rs1: Reg.X, i: Imm32_11_0) extends RV32Instr

  final case class LHU(rd: Reg.X, rs1: Reg.X, i: Imm32_11_0) extends RV32Instr

  final case class SB(rs1: Reg.X, rs2: Reg.X, i: Imm32_11_0) extends RV32Instr

  final case class SH(rs1: Reg.X, rs2: Reg.X, i: Imm32_11_0) extends RV32Instr

  final case class SW(rs1: Reg.X, rs2: Reg.X, i: Imm32_11_0) extends RV32Instr

  final case class ADDI(rd: Reg.X, rs1: Reg.X, i: Imm32_11_0) extends RV32Instr

  final case class SLTI(rd: Reg, rs1: Reg, i: Imm32_11_0) extends RV32Instr

  final case class SLTIU(rd: Reg, rs1: Reg, i: Imm32_11_0) extends RV32Instr

  final case class XORI(rd: Reg.X, rs1: Reg.X, i: Imm32_11_0) extends RV32Instr

  final case class ORI(rd: Reg.X, rs1: Reg.X, i: Imm32_11_0) extends RV32Instr

  final case class ANDI(rd: Reg.X, rs1: Reg.X, i: Imm32_11_0) extends RV32Instr

  final case class SLLI(rd: Reg, rs1: Reg, shiftAmount: U8) extends RV32Instr

  final case class SRLI(rd: Reg, rs1: Reg, shiftAmount: U8) extends RV32Instr

  final case class SRAI(rd: Reg, rs1: Reg, shiftAmount: U8) extends RV32Instr

  final case class ADD(rd: Reg.X, rs1: Reg.X, rs2: Reg.X) extends RV32Instr

  final case class SUB(rd: Reg.X, rs1: Reg.X, rs2: Reg.X) extends RV32Instr

  final case class SLL(rd: Reg.X, rs1: Reg.X, rs2: Reg.X) extends RV32Instr

  final case class SLT(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class SLTU(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class XOR(rd: Reg.X, rs1: Reg.X, rs2: Reg.X) extends RV32Instr

  final case class SRL(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class SRA(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class OR(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class AND(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FENCE(rd: Reg, rs1: Reg, succ: Fin16, pred: Fin16, fm: Fin16) extends RV32Instr

  case object FENCE_TSO extends RV32Instr

  case object PAUSE extends RV32Instr

  case object ECALL extends RV32Instr

  case object EBREAK extends RV32Instr

  // RV32M
  final case class MUL(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class MULH(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class MULHSU(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class MULHU(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class DIV(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class DIVU(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class REM(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class REMU(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  // RV32A
  final case class LR_W(rd: Reg.X, rs1: Reg.X, flag: AQRL) extends RV32Instr

  final case class SC_W(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV32Instr

  final case class AMOSWAP_W(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV32Instr

  final case class AMOADD_W(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV32Instr

  final case class AMOXOR_W(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV32Instr

  final case class AMOAND_W(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV32Instr

  final case class AMOOR_W(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV32Instr

  final case class AMOMIN_W(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV32Instr

  final case class AMOMAX_W(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV32Instr

  final case class AMOMINU_W(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV32Instr

  final case class AMOMAXU_W(rd: Reg.X, rs1: Reg.X, rs2: Reg.X, flag: AQRL) extends RV32Instr

  // RV32F
  final case class FLW(rd: Reg, rs1: Reg, i: Imm32_11_0) extends RV32Instr

  final case class FSW(rs1: Reg, rs2: Reg, i: Imm32_11_0) extends RV32Instr

  final case class FMADD_S(rd: Reg, rs1: Reg, rs2: Reg, src3: Reg, mode: RoundingMode) extends RV32Instr

  final case class FMSUB_S(rd: Reg, rs1: Reg, rs2: Reg, src3: Reg, mode: RoundingMode) extends RV32Instr

  final case class FNMSUB_S(rd: Reg, rs1: Reg, rs2: Reg, src3: Reg, mode: RoundingMode) extends RV32Instr

  final case class FNMADD_S(rd: Reg, rs1: Reg, rs2: Reg, src3: Reg, mode: RoundingMode) extends RV32Instr

  final case class FADD_S(rd: Reg, rs1: Reg, rs2: Reg, src3: Reg, mode: RoundingMode) extends RV32Instr

  final case class FSUB_S(rd: Reg, rs1: Reg, rs2: Reg, mode: RoundingMode) extends RV32Instr

  final case class FMUL_S(rd: Reg, rs1: Reg, rs2: Reg, mode: RoundingMode) extends RV32Instr

  final case class FDIV_S(rd: Reg, rs1: Reg, rs2: Reg, mode: RoundingMode) extends RV32Instr

  final case class FSQRT_S(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV32Instr

  final case class FSGNJ_S(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FSGNJN_S(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FSGNJX_S(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FMIN_S(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FMAX_S(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FCVT_W_S(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV32Instr

  final case class FCVT_WU_S(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV32Instr

  final case class FMV_X_W(rd: Reg, rs1: Reg) extends RV32Instr

  final case class FEQ_S(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FLT_S(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FLE_S(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FCLASS_S(rd: Reg, rs1: Reg) extends RV32Instr

  final case class FCVT_S_W(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV32Instr

  final case class FCVT_S_WU(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV32Instr

  final case class FMV_W_X(rd: Reg, rs1: Reg) extends RV32Instr

  // RV32D
  final case class FLD(rd: Reg, rs1: Reg, i: Imm32_11_0) extends RV32Instr

  final case class FSD(rd: Reg.X, rs1: Reg.F, i: Imm32_11_0) extends RV32Instr

  final case class FMADD_D(rd: Reg, rs1: Reg, rs2: Reg, src3: Reg, mode: RoundingMode) extends RV32Instr

  final case class FMSUB_D(rd: Reg, rs1: Reg, rs2: Reg, src3: Reg, mode: RoundingMode) extends RV32Instr

  final case class FNMSUB_D(rd: Reg, rs1: Reg, rs2: Reg, src3: Reg, mode: RoundingMode) extends RV32Instr

  final case class FNMADD_D(rd: Reg, rs1: Reg, rs2: Reg, src3: Reg, mode: RoundingMode) extends RV32Instr

  final case class FADD_D(rd: Reg, rs1: Reg, rs2: Reg, mode: RoundingMode) extends RV32Instr

  final case class FSUB_D(rd: Reg, rs1: Reg, rs2: Reg, mode: RoundingMode) extends RV32Instr

  final case class FMUL_D(rd: Reg, rs1: Reg, rs2: Reg, mode: RoundingMode) extends RV32Instr

  final case class FDIV_D(rd: Reg, rs1: Reg, rs2: Reg, mode: RoundingMode) extends RV32Instr

  final case class FSQRT_D(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV32Instr

  final case class FSGNJ_D(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FSGNJN_D(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FSGNJX_D(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FMIN_D(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FMAX_D(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FCVT_S_D(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV32Instr

  final case class FCVT_D_S(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV32Instr

  final case class FEQ_D(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FLT_D(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FLE_D(rd: Reg, rs1: Reg, rs2: Reg) extends RV32Instr

  final case class FCLASS_D(rd: Reg, rs1: Reg) extends RV32Instr

  final case class FCVT_W_D(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV32Instr

  final case class FCVT_WU_D(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV32Instr

  final case class FCVT_D_W(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV32Instr

  final case class FCVT_D_WU(rd: Reg, rs1: Reg, mode: RoundingMode) extends RV32Instr
}
