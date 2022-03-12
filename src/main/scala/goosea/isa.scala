package goosea

implicit class Fin32(i: Int) {
  if (!(0<=i && i<32)) {
    throw new IllegalArgumentException()
  }
}
implicit class Fin16(i: Int) {
  if (!(0<=i && i<16)) {
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
final case class Imm32(i: U32)
implicit class Imm32_11_0(i: Imm32)
implicit class Imm32_12_1(i: Imm32)
implicit class Imm32_4_0(i: Imm32)
implicit class Imm32_31_12(i: Imm32)
implicit class Imm32_20_1(i: Imm32)

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

sealed trait RV32Instr
object RV32Instr {
  // RV32I
  final case class LUI(dest: Reg, i: Imm32_31_12) extends RV32Instr
  final case class AUIPC(dest: Reg, i: Imm32_31_12) extends RV32Instr
  final case class JAL(dest: Reg, i: Imm32_20_1) extends RV32Instr
  final case class JALR(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV32Instr
  final case class BEQ(src1: Reg, src2: Reg, i: Imm32_12_1) extends RV32Instr
  final case class BNE(src1: Reg, src2: Reg, i: Imm32_12_1) extends RV32Instr
  final case class BLT(src1: Reg, src2: Reg, i: Imm32_12_1) extends RV32Instr
  final case class BGE(src1: Reg, src2: Reg, i: Imm32_12_1) extends RV32Instr
  final case class BLTU(src1: Reg, src2: Reg, i: Imm32_12_1) extends RV32Instr
  final case class BGEU(src1: Reg, src2: Reg, i: Imm32_12_1) extends RV32Instr
  final case class LB(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV32Instr
  final case class LH(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV32Instr
  final case class LW(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV32Instr
  final case class LBU(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV32Instr
  final case class LHU(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV32Instr
  final case class SB(src1: Reg, src2: Reg, i: Imm32_11_0) extends RV32Instr
  final case class SH(src1: Reg, src2: Reg, i: Imm32_11_0) extends RV32Instr
  final case class SW(src1: Reg, src2: Reg, i: Imm32_11_0) extends RV32Instr
  final case class ADDI(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV32Instr
  final case class SLTI(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV32Instr
  final case class SLTIU(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV32Instr
  final case class XORI(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV32Instr
  final case class ORI(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV32Instr
  final case class ANDI(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV32Instr
  final case class SLLI(dest: Reg, src1: Reg, shiftAmount: U8) extends RV32Instr
  final case class SRLI(dest: Reg, src1: Reg, shiftAmount: U8) extends RV32Instr
  final case class SRAI(dest: Reg, src1: Reg, shiftAmount: U8) extends RV32Instr
  final case class ADD(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class SUB(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class SLL(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class SLT(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class SLTU(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class XOR(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class SRL(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class SRA(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class OR(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class AND(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class FENCE(dest: Reg, src1: Reg, succ: Fin16, pred: Fin16, fm: Fin16) extends RV32Instr
  case object FENCE_TSO extends RV32Instr
  case object PAUSE extends RV32Instr
  case object ECALL extends RV32Instr
  case object EBREAK extends RV32Instr

  // RV32M
  final case class MUL(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class MULH(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class MULHSU(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class MULHU(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class DIV(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class DIVU(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class REM(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr
  final case class REMU(dest: Reg, src1: Reg, src2: Reg) extends RV32Instr

  // RV32A
  final case class LR_W(dest: Reg, src1: Reg, flag: AQRL) extends RV32Instr
  final case class SC_W(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV32Instr
  final case class AMOSWAP_W(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV32Instr
  final case class AMOADD_W(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV32Instr
  final case class AMOXOR_W(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV32Instr
  final case class AMOAND_W(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV32Instr
  final case class AMOOR_W(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV32Instr
  final case class AMOMIN_W(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV32Instr
  final case class AMOMAX_W(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV32Instr
  final case class AMOMINU_W(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV32Instr
  final case class AMOMAXU_W(dest: Reg, src1: Reg, src2: Reg, flag: AQRL) extends RV32Instr

  // RV32F
  final case class FLW(dest: Reg, src1: Reg, i: Imm32_11_0) extends RV32Instr
  final case class FSW(src1: Reg, src2: Reg, i:Imm32_11_0) extends RV32Instr
  final case class FMADD_S(dest:Reg, src1:Reg,src2:Reg,src3:Reg,mode:RoundingMode) extends RV32Instr
  final case class FMSUB_S(dest:Reg, src1:Reg,src2:Reg,src3:Reg,mode:RoundingMode) extends RV32Instr
  final case class FNMSUB_S(dest:Reg, src1:Reg,src2:Reg,src3:Reg,mode:RoundingMode) extends RV32Instr
  final case class FNMADD_S(dest:Reg, src1:Reg,src2:Reg,src3:Reg,mode:RoundingMode) extends RV32Instr
  final case class FADD_S(dest:Reg, src1:Reg,src2:Reg,src3:Reg,mode:RoundingMode) extends RV32Instr
  final case class FSUB_S(dest:Reg, src1:Reg,src2:Reg,mode:RoundingMode) extends RV32Instr
  final case class FMUL_S(dest:Reg, src1:Reg,src2:Reg,mode:RoundingMode) extends RV32Instr
  final case class FDIV_S(dest:Reg, src1:Reg,src2:Reg,mode:RoundingMode) extends RV32Instr
  final case class FSQRT_S(dest:Reg, src1:Reg,mode:RoundingMode) extends RV32Instr
  final case class FSGNJ_S(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FSGNJN_S(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FSGNJX_S(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FMIN_S(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FMAX_S(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FCVT_W_S(dest:Reg, src1:Reg,mode:RoundingMode) extends RV32Instr
  final case class FCVT_WU_S(dest:Reg, src1:Reg,mode:RoundingMode) extends RV32Instr
  final case class FMV_X_W(dest:Reg, src1:Reg) extends RV32Instr
  final case class FEQ_S(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FLT_S(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FLE_S(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FCLASS_S(dest:Reg, src1:Reg) extends RV32Instr
  final case class FCVT_S_W(dest:Reg, src1:Reg,mode:RoundingMode) extends RV32Instr
  final case class FCVT_S_WU(dest:Reg, src1:Reg,mode:RoundingMode) extends RV32Instr
  final case class FMV_W_X(dest:Reg, src1:Reg) extends RV32Instr

  // RV32D
  final case class FLD(dest:Reg,src1:Reg,i:Imm32_11_0) extends RV32Instr
  final case class FSD(dest:Reg,src1:Reg,i:Imm32_11_0) extends RV32Instr
  final case class FMADD_D(dest:Reg, src1:Reg,src2:Reg,src3:Reg,mode:RoundingMode) extends RV32Instr
  final case class FMSUB_D(dest:Reg, src1:Reg,src2:Reg,src3:Reg,mode:RoundingMode) extends RV32Instr
  final case class FNMSUB_D(dest:Reg, src1:Reg,src2:Reg,src3:Reg,mode:RoundingMode) extends RV32Instr
  final case class FNMADD_D(dest:Reg, src1:Reg,src2:Reg,src3:Reg,mode:RoundingMode) extends RV32Instr
  final case class FADD_D(dest:Reg, src1:Reg,src2:Reg,mode:RoundingMode) extends RV32Instr
  final case class FSUB_D(dest:Reg, src1:Reg,src2:Reg,mode:RoundingMode) extends RV32Instr
  final case class FMUL_D(dest:Reg, src1:Reg,src2:Reg,mode:RoundingMode) extends RV32Instr
  final case class FDIV_D(dest:Reg, src1:Reg,src2:Reg,mode:RoundingMode) extends RV32Instr
  final case class FSQRT_D(dest:Reg, src1:Reg,mode:RoundingMode) extends RV32Instr
  final case class FSGNJ_D(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FSGNJN_D(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FSGNJX_D(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FMIN_D(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FMAX_D(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FCVT_S_D(dest:Reg, src1:Reg,mode:RoundingMode) extends RV32Instr
  final case class FCVT_D_S(dest:Reg, src1:Reg,mode:RoundingMode) extends RV32Instr
  final case class FEQ_D(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FLT_D(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FLE_D(dest:Reg, src1:Reg,src2:Reg) extends RV32Instr
  final case class FCLASS_D(dest:Reg, src1:Reg) extends RV32Instr
  final case class FCVT_W_D(dest:Reg, src1:Reg,mode:RoundingMode) extends RV32Instr
  final case class FCVT_WU_D(dest:Reg, src1:Reg,mode:RoundingMode) extends RV32Instr
  final case class FCVT_D_W(dest:Reg, src1:Reg,mode:RoundingMode) extends RV32Instr
  final case class FCVT_D_WU(dest:Reg, src1:Reg,mode:RoundingMode) extends RV32Instr
}
