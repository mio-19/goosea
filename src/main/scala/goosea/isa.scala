package goosea

implicit class Fin32(i: Int) {
  if (!(0<=i && i<32)) {
    throw new IllegalArgumentException()
  }
}

implicit class U32(i: Int) {
  // todo
}

implicit class U8(i:Short)

sealed trait Reg
object Reg {
  final case class X(f: Fin32) extends Reg
  final case class F(f:Fin32) extends Reg
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
sealed abstract class Imm32(i: U32)
final case class Imm32_11_0(i: U32) extends Imm32(i)
final case class Imm32_4_0(i:U32) extends Imm32(i)

// Atomic instruction flag: Acquire and Release
final case class AQRL(acquire: Boolean, release: Boolean)

sealed trait RV64Instr
object RV64Instr {
  // RV64I
  final case class LWU(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV64Instr
  final case class LD(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV64Instr
  final case class SD(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV64Instr
  final case class SLLI(dest: Reg, src1: Reg, shiftAmount: U8) extends RV64Instr
  final case class SRLI(dest: Reg, src1: Reg, shiftAmount: U8) extends RV64Instr
  final case class SRAI(dest: Reg, src1: Reg, shiftAmount: U8) extends RV64Instr
  final case class ADDIW(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV64Instr
  final case class SLLIW(dest: Reg, src1: Reg, shiftAmount: U8) extends RV64Instr
  final case class SRLIW(dest: Reg, src1: Reg, shiftAmount: U8) extends RV64Instr
  final case class SRAIW(dest: Reg, src1: Reg, shiftAmount: U8) extends RV64Instr
  final case class ADDW(dest: Reg, src1: Reg, src2: Reg) extends RV64Instr
  final case class SUBW(dest: Reg, src1: Reg, src2: Reg) extends RV64Instr
  final case class SLLW(dest: Reg, src1: Reg, src2: Reg) extends RV64Instr
  final case class SRLW(dest: Reg, src1: Reg, src2: Reg) extends RV64Instr
  final case class SRAW(dest: Reg, src1: Reg, src2: Reg) extends RV64Instr

  // RV64M
  final case class MULW(dest: Reg, src1: Reg, src2: Reg) extends RV64Instr
  final case class DIVW(dest: Reg, src1: Reg, src2: Reg) extends RV64Instr
  final case class DIVUW(dest: Reg, src1: Reg, src2: Reg) extends RV64Instr
  final case class REMW(dest: Reg, src1: Reg, src2: Reg) extends RV64Instr
  final case class REMUW(dest: Reg, src1: Reg, src2: Reg) extends RV64Instr

  // RV64A
  final case class LR_D(dest: Reg, src1: Reg, flag: AQRL) extends RV64Instr
  final case class SC_D(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV64Instr
  final case class AMOSWAP_D(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV64Instr
  final case class AMOADD_D(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV64Instr
  final case class AMOXOR_D(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV64Instr
  final case class AMOAND_D(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV64Instr
  final case class AMOOR_D(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV64Instr
  final case class AMOMIN_D(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV64Instr
  final case class AMOMAX_D(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV64Instr
  final case class AMOMINU_D(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV64Instr
  final case class AMOMAXU_D(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV64Instr

  // RV64F
  final case class FCVT_L_S(dest: Reg, src1: Reg, mode: RoundingMode) extends RV64Instr
  final case class FCVT_LU_S(dest: Reg, src1: Reg, mode: RoundingMode) extends RV64Instr
  final case class FCVT_S_L(dest: Reg, src1: Reg, mode: RoundingMode) extends RV64Instr
  final case class FCVT_S_LU(dest: Reg, src1: Reg, mode: RoundingMode) extends RV64Instr

  // RV64D
  final case class FCVT_L_D(dest: Reg, src1: Reg, mode: RoundingMode) extends RV64Instr
  final case class FCVT_LU_D(dest: Reg, src1: Reg, mode: RoundingMode) extends RV64Instr
  final case class FMV_X_D(dest: Reg, src1: Reg) extends RV64Instr
  final case class FCVT_D_L(dest: Reg, src1: Reg, mode: RoundingMode) extends RV64Instr
  final case class FCVT_D_LU(dest: Reg, src1: Reg, mode: RoundingMode) extends RV64Instr
  final case class FMV_D_X(dest: Reg, src1: Reg) extends RV64Instr

  // RV32/64 Zicsr
  final case class CSRRW(dest: Reg, src1: Reg, csr: Imm32_11_0) extends RV64Instr
  final case class CSRRS(dest: Reg, src1: Reg, csr: Imm32_11_0) extends RV64Instr
  final case class CSRRC(dest: Reg, src1: Reg, csr: Imm32_11_0) extends RV64Instr
  final case class CSRRWI(dest: Reg, i: Imm32_4_0, csr: Imm32_11_0) extends RV64Instr
  final case class CSRRSI(dest: Reg, i: Imm32_4_0, csr: Imm32_11_0) extends RV64Instr
  final case class CSRRCI(dest: Reg, i: Imm32_4_0, csr: Imm32_11_0) extends RV64Instr

  // RV32/64 Zifencei
  final case class FENCE_I(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV64Instr
}
