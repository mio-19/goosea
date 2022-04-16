package goosea.isa.compressed

import goosea.isa._
import goosea.utils._
import goosea.isa.untyped.gp



def fp_3(reg: Int): Reg = if (0 <= reg && reg < 8) Reg.F(reg+8) else throw new IllegalArgumentException()
def gp_3(reg: Int): Reg = if (0 <= reg && reg < 8) Reg.X(reg+8) else throw new IllegalArgumentException()


def hint = NOP
def nzimm_is_0(untyped:Bytecode16) = untyped.ci.imm_b5 == 0 && untyped.ci.imm_b1 == 0
def nzimm_not_0(untyped:Bytecode16) = !nzimm_is_0(untyped)
def rd_is(untyped:Bytecode16, n:Int) = untyped.ci.rd == n
def rd_is_0(untyped:Bytecode16) = rd_is(untyped, 0)
def arith[T](op:(rd: Reg, rs1: Reg, rs2: Reg)=>T, untyped:Bytecode16) = {
  val ca = untyped.ca
  val rd = gp_3(ca.rd_or_rs1)
  val rs2 = gp_3(ca.rs2)
}

object Instr {
  def try_from_compressed(untyped:Bytecode16):Option[Instr] = {
    val repr = untyped.repr
    ((repr, repr & 3) : (Int, Int)) match {
      case (0, _) => None
      case (_, 0)|(_, 1)|(_, 2) => decode_untyped(untyped)
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
    }
    case _ => return None // todo
  })
}

def extendi8(imm: Int) = {
  val i8 = (0xc0 | imm)
  if ((i8 & (1 << 7)) == 0) i8 else i8 | 0xffffff00
}