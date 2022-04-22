package goosea.isa.compressed

import goosea.isa._
import goosea.utils._
import goosea.isa.untyped.{gp, fp}
import scala.language.implicitConversions


def fp_3(reg: Int): Reg.F = if (0 <= reg && reg < 8) Reg.F(reg + 8) else throw new IllegalArgumentException()
def gp_3(reg: Int): Reg.X = if (0 <= reg && reg < 8) Reg.X(reg + 8) else throw new IllegalArgumentException()


def hint = NOP
def nzimm_is_0(untyped: Bytecode16) = untyped.ci.imm_b5 == 0 && untyped.ci.imm_b1 == 0
def nzimm_not_0(untyped: Bytecode16) = !nzimm_is_0(untyped)
def rd_is(untyped: Bytecode16, n: Int) = untyped.ci.rd == n
def rd_is_0(untyped: Bytecode16) = rd_is(untyped, 0)
def arith[T, R >: Reg.X](op: (rd: R, rs1: R, rs2: R) => T, untyped: Bytecode16) = {
  val ca = untyped.ca
  val rd = gp_3(ca.rd_or_rs1)
  val rs2 = gp_3(ca.rs2)
  op(rd, rd, rs2)
}

object Instr {
  def try_from_compressed(untyped: Bytecode16): Option[Instr] = {
    val repr = untyped.repr
    ((repr, repr & 3): (Int, Int)) match {
      case (0, _) => None
      case (_, 0) | (_, 1) | (_, 2) => decode_untyped(untyped)
      case _ => None
    }
  }
}

def decode_untyped(untyped: Bytecode16): Option[Instr] = {
  val inst = untyped.repr
  Some(untyped.opcode match {
    case 0 => untyped.funct3 match {
      // C.ADDI4SPN (RES for nzuimm = 0)
      case 0 if untyped.ciw.imm_b8 == 0 => return None // reserved
      case 0 => {
        val ciw = untyped.ciw
        val rd = gp_3(ciw.rd)
        // nzuimm[5:4|9:6|2|3] = inst[12:11|10:7|6|5]
        val nzuimm = ((inst >> 1) & 0x3c0) // nzuimm[9:6]
          | ((inst >> 7) & 0x30) // nzuimm[5:4]
          | ((inst >> 2) & 0x8) // nzuimm[3]
          | ((inst >> 4) & 0x4); // nzuimm[2]
        RV32Instr.ADDI(rd, Reg.X(2), Imm32_11_0(nzuimm))
      }

      // C.FLD for RV32/64, C.LQ for RV128 (not supported)
      case 1 => {
        val cl = untyped.cl
        val rd = fp_3(cl.rd)
        val rs1 = gp_3(cl.rs1)
        // offset[5:3|7:6] = isnt[12:10|6:5]
        val offset = ((inst << 1) & 0xc0) // imm[7:6]
          | ((inst >> 7) & 0x38); // imm[5:3]
        RV32Instr.FLD(rd, rs1, Imm32_11_0(offset))
      }

      // C.LW
      case 2 => {
        val cl = untyped.cl
        val rd = gp_3(cl.rd)
        val rs1 = gp_3(cl.rs1)
        // offset[5:3|2|6] = isnt[12:10|6|5]
        val offset = ((inst << 1) & 0x40) // imm[6]
          | ((inst >> 7) & 0x38) // imm[5:3]
          | ((inst >> 4) & 0x4); // imm[2]
        RV32Instr.LW(rd, rs1, Imm32_11_0(offset))
      }

      // C.LD for RV64/128, C.FLW for RV32 (not supported)
      case 3 => {
        val cl = untyped.cl
        val rd = fp_3(cl.rd)
        val rs1 = gp_3(cl.rs1)
        // offset[5:3|7:6] = isnt[12:10|6:5]
        val offset = ((inst << 1) & 0xc0) // imm[7:6]
          | ((inst >> 7) & 0x38); // imm[5:3]
        RV64Instr.LD(rd, rs1, Imm32_11_0(offset))
      }

      case 4 => return None // reserved

      // C.FSD for RV32/64, C.SQ for RV128 (not supported)
      case 5 => {
        val cs = untyped.cs
        val rs1 = gp_3(cs.rs1)
        val rs2 = fp_3(cs.rs2)
        // offset[5:3|7:6] = isnt[12:10|6:5]
        val offset = ((inst << 1) & 0xc0) // imm[7:6]
          | ((inst >> 7) & 0x38); // imm[5:3]
        RV32Instr.FSD(rs1, rs2, Imm32_11_0(offset))
      }

      // C.SW
      case 6 => {
        val cs = untyped.cs
        val rs1 = gp_3(cs.rs1)
        val rs2 = gp_3(cs.rs2)
        // offset[5:3|2|6] = isnt[12:10|6|5]
        val offset = ((inst << 1) & 0x40) // imm[6]
          | ((inst >> 7) & 0x38) // imm[5:3]
          | ((inst >> 4) & 0x4); // imm[2]
        RV32Instr.SW(rs1, rs2, Imm32_11_0(offset))
      }

      // C.SD for RV64/128, C.FSW for RV32 (not supported)
      case 7 => {
        val cs = untyped.cs
        val rs1 = gp_3(cs.rs1)
        val rs2 = gp_3(cs.rs2)
        // offset[5:3|7:6] = isnt[12:10|6:5]
        val offset = ((inst << 1) & 0xc0) // imm[7:6]
          | ((inst >> 7) & 0x38); // imm[5:3]
        RV64Instr.SD(rs1, rs2, Imm32_11_0(offset))
      }
      case _ => return None
    }
    case 1 => untyped.funct3 match {
      // C.NOP
      case 0 if rd_is_0(untyped) && nzimm_not_0(untyped) => hint
      case 0 if rd_is_0(untyped) => NOP
      // C.ADDI
      case 0 if nzimm_is_0(untyped) => hint
      case 0 => {
        val ci = untyped.ci
        val rd = gp(ci.rd)
        // imm[5|4:0] = inst[12|6:2]
        val imm = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f)
        // Sign-extended.
        val imm1 = (imm & 0x20) == 0 match {
          case true => imm
          case false => extendi8(0xc0 | imm)
        }
        RV32Instr.ADDI(rd, rd, Imm32_11_0(imm1))
      }

      // C.ADDIW for RV64/128 (RES for rd = 0), C.JAL for RV32 (not supported)
      case 1 if rd_is_0(untyped) => return None // reserved
      case 1 => {
        val ci = untyped.ci
        val rd = gp(ci.rd)
        // imm[5|4:0] = inst[12|6:2]
        val imm = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f)
        // Sign-extended.
        val imm1 = (imm & 0x20) == 0 match {
          case true => imm
          case false => extendi8(0xc0 | imm)
        }
        RV64Instr.ADDIW(rd, rd, Imm32_11_0(imm1))
      }

      // C.LI
      case 2 => {
        val ci = untyped.ci
        val rd = gp(ci.rd)
        // imm[5|4:0] = inst[12|6:2]
        val imm = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f)
        // Sign-extended.
        val imm1 = (imm & 0x20) == 0 match {
          case true => imm
          case false => extendi8(0xc0 | imm)
        }
        RV32Instr.ADDI(rd, Reg.X(0), Imm32_11_0(imm1))
      }

      // C.ADDI16SP (RES for nzimm = 0)
      case 3 if rd_is(untyped, 2) && nzimm_is_0(untyped) => return None // reserved
      case 3 if rd_is(untyped, 2) => {
        // nzimm[9|4|6|8:7|5] = inst[12|6|5|4:3|2]
        val nzimm = ((inst >> 3) & 0x200) // nzimm[9]
          | ((inst >> 2) & 0x10) // nzimm[4]
          | ((inst << 1) & 0x40) // nzimm[6]
          | ((inst << 4) & 0x180) // nzimm[8:7]
          | ((inst << 3) & 0x20) // nzimm[5]
        // Sign-extended.
        val nzimm1 = (nzimm & 0x200) == 0 match {
          case true => nzimm
          case false => extendi16(0xfc00 | nzimm)
        }
        RV32Instr.ADDI(Reg.X(2), Reg.X(2), Imm32_11_0(nzimm1))
      }
      // C.LUI (RES for imm = 0; HINT for rd = 0)
      case 3 if nzimm_is_0(untyped) => return None // reserved
      case 3 if rd_is_0(untyped) => hint
      case 3 => {
        val ci = untyped.ci
        val rd = gp(ci.rd)
        // imm[17|16:12] = inst[12|6:2]
        val imm = ((U32(inst) << 5) & 0x20000) | ((U32(inst) << 10) & 0x1f000)
        // Sign-extended.
        val imm1 = (imm & 0x20000) == 0 match {
          case true => imm
          case false => (imm|0xfffc0000 )
        }
        RV32Instr.LUI(rd, Imm32_31_12(imm1 >> 12))
      }
      case 4 => untyped.cbi.funct2 match {
        // HINT for RV32/64, C.SRLI64 for RV128 (not supported)
        case 0 if nzimm_is_0(untyped) => hint

        // C.SRLI
        case 0 => {
          val cbi = untyped.cbi
          val rd = gp_3(cbi.rd_or_rs1)
          // shamt[5|4:0] = inst[12|6:2]
          val shamt = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f)
          RV64Instr.SRLI(rd, rd, shamt)
        }

        // HINT for RV32/64, C.SRAI64 for RV128 (not supported)
        case 1 if nzimm_is_0(untyped) => hint
        // C.SRAI
        case 1 => {
          val cbi = untyped.cbi
          val rd = gp_3(cbi.rd_or_rs1)
          // shamt[5|4:0] = inst[12|6:2]
          val shamt = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f)
          RV64Instr.SRAI(rd, rd, shamt)
        }

        // C.ANDI
        case 2 => {
          val cbi = untyped.cbi
          val rd = gp_3(cbi.rd_or_rs1)
          // imm[5|4:0] = inst[12|6:2]
          val imm = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f)
          // Sign-extended.
          val imm1 = (imm & 0x20) == 0 match {
            case true => imm
            case false => extendi8(0xc0 | imm)
          }
          RV32Instr.ANDI(rd, rd, Imm32_11_0(imm1))
        }
        case 3 => (untyped.ca.funct6, untyped.ca.funct2) match {
          // C.SUB
          case (0x23, 0) => arith(RV32Instr.SUB, untyped)
          // C.XOR
          case (0x23, 1) => arith(RV32Instr.XOR, untyped)
          // C.OR
          case (0x23, 2) => arith(RV32Instr.OR, untyped)
          // C.AND
          case (0x23, 3) => arith(RV32Instr.AND, untyped)
          // C.SUBW
          case (0x27, 0) => arith(RV64Instr.SUBW, untyped)
          // C.ADDW
          case (0x27, 1) => arith(RV64Instr.ADDW, untyped)
          // Reserved
          case (0x27, 2) => return None // reserved
          // Reserved
          case (0x27, 3) => return None // reserved
          case _ => return None
        }
        case _ => return None
      }
      // C.J
      case 5 => {
        // offset[11|4|9:8|10|6|7|3:1|5] = inst[12|11|10:9|8|7|6|5:3|2]
        val offset = ((inst >> 1) & 0x800) // offset[11]
          | ((inst << 2) & 0x400) // offset[10]
          | ((inst >> 1) & 0x300) // offset[9:8]
          | ((inst << 1) & 0x80) // offset[7]
          | ((inst >> 1) & 0x40) // offset[6]
          | ((inst << 3) & 0x20) // offset[5]
          | ((inst >> 7) & 0x10) // offset[4]
          | ((inst >> 2) & 0xe) // offset[3:1]
        // Sign-extended.
        val offset1 = (offset & 0x800) == 0 match {
          case true => offset
          case false => (0xf000 | offset)
        }
        RV32Instr.JAL(Reg.X(0), Imm32_20_1(offset1 >> 1))
      }
      // C.BEQZ
      case 6 => {
        val cb = untyped.cb
        val rs1 = gp_3(cb.rd_or_rs1)
        // offset[8|4:3|7:6|2:1|5] = inst[12|11:10|6:5|4:3|2]
        val offset = ((inst >> 4) & 0x100) // offset[8]
          | ((inst << 1) & 0xc0) // offset[7:6]
          | ((inst << 3) & 0x20) // offset[5]
          | ((inst >> 7) & 0x18) // offset[4:3]
          | ((inst >> 2) & 0x6) // offset[2:1]
        // Sign-extended.
        val offset1 = (offset & 0x100) == 0 match {
          case true => offset
          case false => (0xfe00 | offset)
        }
        RV32Instr.BEQ(rs1, Reg.X(0), Imm32_12_1(offset1 >> 1))
      }
      // C.BNEZ
      case 7 => {
        val cb = untyped.cb
        val rs1 = gp_3(cb.rd_or_rs1)
        // offset[8|4:3|7:6|2:1|5] = inst[12|11:10|6:5|4:3|2]
        val offset = ((inst >> 4) & 0x100) // offset[8]
          | ((inst << 1) & 0xc0) // offset[7:6]
          | ((inst << 3) & 0x20) // offset[5]
          | ((inst >> 7) & 0x18) // offset[4:3]
          | ((inst >> 2) & 0x6) // offset[2:1]
        // Sign-extended.
        val offset1 = (offset & 0x100) == 0 match {
          case true => offset
          case false => (0xfe00 | offset)
        }
        RV32Instr.BNE(rs1, Reg.X(0), Imm32_12_1(offset1 >> 1))
      }
      case _ => return None
    }
    case 2 => untyped.funct3 match {
      // C.SLLI
      case 0 if rd_is_0(untyped) => hint
      case 0 if nzimm_is_0(untyped) => hint
      case 0 => {
        val ci = untyped.ci
        val rd = gp(ci.rd)
        // shamt[5|4:0] = inst[12|6:2]
        val shamt = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f)
        RV64Instr.SLLI(rd, rd, shamt)
      }
      // C.FLDSP for RV32/64, C.LQSP for RV128 (not supported)
      case 1 => {
        val ci = untyped.ci
        val rd = fp(ci.rd)
        // offset[5|4:3|8:6] = inst[12|6:5|4:2]
        val offset = ((inst << 4) & 0x1c0) // offset[8:6]
          | ((inst >> 7) & 0x20) // offset[5]
          | ((inst >> 2) & 0x18) // offset[4:3]
        RV32Instr.FLD(rd, Reg.X(2), Imm32_11_0(offset))
      }
      // C.LWSP (RES for rd = 0)
      case 2 if rd_is_0(untyped) => return None // reserved
      case 2 => {
        val ci = untyped.ci
        val rd = gp(ci.rd)
        // offset[5|4:2|7:6] = inst[12|6:4|3:2]
        val offset = ((inst << 4) & 0xc0) // offset[7:6]
          | ((inst >> 7) & 0x20) // offset[5]
          | ((inst >> 2) & 0x1c) // offset[4:2]
        RV32Instr.LW(rd, Reg.X(2), Imm32_11_0(offset))
      }
      // C.LDSP for RV64/128 (RES for rd = 0), C.FLWSP for RV32 (not supported)
      case 3 if rd_is_0(untyped) => return None // reserved
      case 3 => {
        val ci = untyped.ci
        val rd = fp(ci.rd)
        // offset[5|4:3|8:6] = inst[12|6:5|4:2]
        val offset = ((inst << 4) & 0x1c0) // offset[8:6]
          | ((inst >> 7) & 0x20) // offset[5]
          | ((inst >> 2) & 0x18) // offset[4:3]
        RV64Instr.LD(rd, Reg.X(2), Imm32_11_0(offset))
      }
      // C.JR (RES for rs1/rd = 0)
      case 4 if nzimm_is_0(untyped) && rd_is_0(untyped) => return None // reserved
      case 4 if nzimm_is_0(untyped) => {
        val cr = untyped.cr
        val rs1 = gp(cr.rd_or_rs1)
        RV32Instr.JALR(Reg.X(0), rs1, Imm32_11_0(0))
      }
      // C.MV
      case 4 if untyped.ci.imm_b1 == 0 && untyped.ci.imm_b5 != 0 && rd_is_0(untyped) => hint
      case 4 if untyped.ci.imm_b1 == 0 && untyped.ci.imm_b5 != 0 => {
        val cr = untyped.cr
        val rd = gp(cr.rd_or_rs1)
        val rs2 = gp(cr.rs2)
        RV32Instr.ADD(rd, Reg.X(0), rs2)
      }
      // C.EBREAK
      case 4 if untyped.repr == 0x9002 => RV32Instr.EBREAK
      // C.JALR
      case 4 if untyped.ci.imm_b1 == 1 && untyped.ci.imm_b5 == 0 => {
        val cr = untyped.cr
        val rs1 = gp(cr.rd_or_rs1)
        RV32Instr.JALR(Reg.X(1), rs1, Imm32_11_0(0))
      }
      // C.ADD
      case 4 if untyped.ci.imm_b1 == 1 => {
        val cr = untyped.cr
        val rd = gp(cr.rd_or_rs1)
        val rs2 = gp(cr.rs2)
        RV32Instr.ADD(rd, rd, rs2)
      }
      // C.FSDSP for RV32/64, C.SQSP for RV128 (not supported)
      case 5 => {
        val css = untyped.css
        val rs2 = fp(css.rs2)
        // offset[5:3|8:6] = isnt[12:10|9:7]
        val offset = ((inst >> 1) & 0x1c0) // offset[8:6]
          | ((inst >> 7) & 0x38) // offset[5:3]
        RV32Instr.FSD(Reg.X(2), rs2, Imm32_11_0(offset))
      }
      // C.SWSP
      case 6 => {
        val css = untyped.css
        val rs2 = gp(css.rs2)
        // offset[5:2|7:6] = inst[12:9|8:7]
        val offset = ((inst >> 1) & 0xc0) // offset[7:6]
          | ((inst >> 7) & 0x3c) // offset[5:2]
        RV32Instr.SW(Reg.X(2), rs2, Imm32_11_0(offset))
      }
      // C.SDSP for RV64/128, C.FSWSP for RV32 (not supported)
      case 7 => {
        val css = untyped.css
        val rs2 = gp(css.rs2)
        // offset[5:3|8:6] = isnt[12:10|9:7]
        val offset = ((inst >> 1) & 0x1c0) // offset[8:6]
          | ((inst >> 7) & 0x38) // offset[5:3]
        RV64Instr.SD(Reg.X(2), rs2, Imm32_11_0(offset))
      }
      case _ => return None
    }
    case _ => return None
  })
}

def extendi8(i8: Int) = {
  if ((i8 & (1 << 7)) == 0) i8 else i8 | 0xffffff00
}
def extendi16(i16: Int) = {
  if ((i16 & (1 << 15)) == 0) i16 else i16 | 0xffff0000
}