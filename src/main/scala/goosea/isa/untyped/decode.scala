package goosea.isa.untyped

import goosea.isa._
import goosea.utils._

import scala.language.implicitConversions

def fp(reg: Int): Reg = if (0 <= reg && reg < 32) Reg.F(reg) else throw new IllegalArgumentException()
def gp(reg: Int): Reg = if (0 <= reg && reg < 32) Reg.X(reg) else throw new IllegalArgumentException()

// Opcode map, with `inst[1:0]=11` stripped away.
object OpcodeMap {
  val LOAD = 0
  val LOAD_FP = 1
  val _custom_0 = 2
  val MISC_MEM = 3
  val OP_IMM = 4
  val AUIPC = 5
  val OP_IMM_32 = 6
  // no 7
  val STORE = 8
  val STORE_FP = 9
  val _custom_1 = 10
  val AMO = 11
  val OP = 12
  val LUI = 13
  val OP_32 = 14
  // no 15
  val MADD = 16
  val MSUB = 17
  val NMSUB = 18
  val NMADD = 19
  val OP_FP = 20
  val _reversed_0 = 21
  val _custom_2_or_rv128 = 22
  // no 23
  val BRANCH = 24
  val JALR = 25
  val _reversed_1 = 26
  val JAL = 27
  val SYSTEM = 28
  val _reversed_2 = 29
  val _custom_3_or_rv128 = 30
}

private def u[T](opcode: (Reg, Imm32_31_12) => T, untyped: Bytecode, reg: Int => Reg): T = {
  val ut = untyped.u
  opcode(reg(ut.rd), Imm32_31_12(ut.imm31_12))
}
private def j[T](opcode: (Reg, Imm32_20_1) => T, untyped: Bytecode, reg: Int => Reg): T = {
  val jt = untyped.j

  val imm20 = Imm32_20_20(jt.imm20)
  val imm10_1 = Imm32_10_1(jt.imm10_1)
  val imm11 = Imm32_11_11(jt.imm11)
  val imm19_12 = Imm32_19_12(jt.imm19_12)
  val imm = imm20.bitor(imm19_12).bitor(imm11).bitor(imm10_1)
  opcode(reg(jt.rd), Imm32_20_1.from(imm))
}
private def i[T](opcode: (Reg, Reg, Imm32_11_0) => T, untyped: Bytecode, reg: Int => Reg): T = {
  val it = untyped.i
  opcode(reg(it.rd), reg(it.rs1), Imm32_11_0(it.imm11_0))
}
private def i_load_fp[T](opcode: (Reg, Reg, Imm32_11_0) => T, untyped: Bytecode, freg: Int => Reg,greg:Int=>Reg): T = {
  val it = untyped.i
  opcode(freg(it.rd), greg(it.rs1), Imm32_11_0(it.imm11_0))
}
private def b[T](opcode: (Reg, Reg, Imm32_12_1) => T, untyped: Bytecode, reg: Int => Reg): T = {
  val bt = untyped.b
  val imm12 = Imm32_12_12(bt.imm12)
  val imm10_5 = Imm32_10_5(bt.imm10_5)
  val imm11 = Imm32_11_11(bt.imm11)
  val imm4_1 = Imm32_4_1(bt.imm4_1)
  val imm = imm12.bitor(imm11).bitor(imm10_5).bitor(imm4_1)
  opcode(reg(bt.rs1), reg(bt.rs2), Imm32_12_1.from(imm))
}
private def s[T](opcode: (Reg, Reg, Imm32_11_0) => T, untyped: Bytecode, reg: Int => Reg): T = {
  val st = untyped.s
  val imm11_5 = Imm32_11_5(st.imm11_5)
  val imm4_0 = Imm32_4_0(st.imm4_0)
  val imm = imm11_5.bitor(imm4_0)
  opcode(reg(st.rs1), reg(st.rs2), Imm32_11_0.from(imm))
}
private def s_store_fp[T](opcode: (Reg, Reg, Imm32_11_0) => T, untyped: Bytecode, freg: Int => Reg,greg: Int => Reg): T = {
  val st = untyped.s
  val imm11_5 = Imm32_11_5(st.imm11_5)
  val imm4_0 = Imm32_4_0(st.imm4_0)
  val imm = imm11_5.bitor(imm4_0)
  opcode(greg(st.rs1), freg(st.rs2), Imm32_11_0.from(imm))
}
private def rshamt32[T](opcode: (Reg, Reg, U8) => T, untyped: Bytecode, reg: Int => Reg): T = {
  val rt = untyped.rshamt32
  opcode(reg(rt.rd), reg(rt.rs1), U8(rt.shamt))
}
private def rshamt64[T](opcode: (Reg, Reg, U8) => T, untyped: Bytecode, reg: Int => Reg): T = {
  val rt = untyped.rshamt64
  opcode(reg(rt.rd), reg(rt.rs1), U8(rt.shamt))
}
private def r[T](opcode: (Reg, Reg, Reg) => T, untyped: Bytecode, reg: Int => Reg): T = {
  val rt = untyped.r
  opcode(reg(rt.rd), reg(rt.rs1), reg(rt.rs2))
}
private def fence[T](opcode: (rd: Reg, rs1: Reg, succ: Fin16, pred: Fin16, fm: Fin16) => T, untyped: Bytecode, reg: Int => Reg): T = {
  val ft = untyped.fence
  opcode(reg(ft.rd), reg(ft.rs1), Fin16(ft.succ), Fin16(ft.pred), Fin16(ft.fm))
}
private def r_no_rd[T](opcode: (Reg, Reg) => T, untyped: Bytecode, reg: Int => Reg): T = {
  val rt = untyped.r
  opcode(reg(rt.rs1), reg(rt.rs2))
}
private def zicsr_rs1[T](opcode: (rd: Reg, rs1: Reg, csr: Imm32_11_0) => T, untyped: Bytecode, reg: Int => Reg): T = {
  val it = untyped.i
  opcode(reg(it.rd), reg(it.rs1), Imm32_11_0(it.imm11_0))
}
private def zicsr_uimm[T](opcode: (rd: Reg, i: Imm32_4_0, csr: Imm32_11_0) => T, untyped: Bytecode, reg: Int => Reg): T = {
  val it = untyped.i
  opcode(reg(it.rd), Imm32_4_0(it.rs1), Imm32_11_0(it.imm11_0))
}
private def ra[T](opcode: (rd: Reg, rs1: Reg, rs2: Reg, flag: AQRL) => T, untyped: Bytecode, reg: Int => Reg): T = {
  val rt = untyped.ra
  opcode(reg(rt.rd), reg(rt.rs1), reg(rt.rs2), AQRL(rt.aq, rt.rl))
}
private def ra_only_rs1[T](opcode: (rd: Reg, rs1: Reg, flag: AQRL) => T, untyped: Bytecode, reg: Int => Reg): T = {
  val rt = untyped.ra
  if (rt.rs2 != 0) throw new IllegalArgumentException()
  opcode(reg(rt.rd), reg(rt.rs1), AQRL(rt.aq, rt.rl))
}

def decode(untyped: Bytecode): Option[Instr] = {
  val opcode = untyped.opcode >> 2 // stripping away `inst[1:0]=11`
  Some(opcode match {
    case OpcodeMap.LUI => u(RV32Instr.LUI, untyped, gp)
    case OpcodeMap.AUIPC => u(RV32Instr.AUIPC, untyped, gp)
    case OpcodeMap.JAL => j(RV32Instr.JAL, untyped, gp)
    case OpcodeMap.JALR => i(RV32Instr.JALR, untyped, gp)
    case OpcodeMap.BRANCH => untyped.b.funct3 match {
      case 0 => b(RV32Instr.BEQ, untyped, gp)
      case 1 => b(RV32Instr.BNE, untyped, gp)
      case 4 => b(RV32Instr.BLT, untyped, gp)
      case 5 => b(RV32Instr.BGE, untyped, gp)
      case 6 => b(RV32Instr.BLTU, untyped, gp)
      case 7 => b(RV32Instr.BGEU, untyped, gp)
      case _ => return None
    }
    case OpcodeMap.LOAD => untyped.i.funct3 match {
      case 0 => i(RV32Instr.LB, untyped, gp)
      case 1 => i(RV32Instr.LH, untyped, gp)
      case 2 => i(RV32Instr.LW, untyped, gp)
      case 4 => i(RV32Instr.LBU, untyped, gp)
      case 5 => i(RV32Instr.LHU, untyped, gp)
      case 6 => i(RV64Instr.LWU, untyped, gp)
      case 3 => i(RV64Instr.LD, untyped, gp)
      case _ => return None
    }
    case OpcodeMap.STORE => untyped.s.funct3 match {
      case 0 => s(RV32Instr.SB, untyped, gp)
      case 1 => s(RV32Instr.SH, untyped, gp)
      case 2 => s(RV32Instr.SW, untyped, gp)
      case 3 => s(RV64Instr.SD, untyped, gp)
      case _ => return None
    }
    case OpcodeMap.OP_IMM => untyped.i.funct3 match {
      case 0 => i(RV32Instr.ADDI, untyped, gp)
      case 2 => i(RV32Instr.SLTI, untyped, gp)
      case 3 => i(RV32Instr.SLTIU, untyped, gp)
      case 4 => i(RV32Instr.XORI, untyped, gp)
      case 6 => i(RV32Instr.ORI, untyped, gp)
      case 7 => i(RV32Instr.ANDI, untyped, gp)
      // RV64's SLLI, SRLI, SRAI have a 1-bit-more `shamt` field compared to RV32:
      // The `shamt` field in RV32: 5 bits
      // The `shamt` field in RV64: 6 bits
      case 1 => rshamt64(RV64Instr.SLLI, untyped, gp)
      case 5 => untyped.rshamt64.funct6 match {
        case 0 => rshamt64(RV64Instr.SRLI, untyped, gp)
        case 32 => rshamt64(RV64Instr.SRAI, untyped, gp)
        case _ => return None
      }
      case _ => return None
    }
    case OpcodeMap.OP_IMM_32 => untyped.i.funct3 match {
      case 0 => i(RV64Instr.ADDIW, untyped, gp)
      case 1 => rshamt32(RV64Instr.SLLIW, untyped, gp)
      case 5 => untyped.rshamt32.funct7 match {
        case 0 => rshamt32(RV64Instr.SRLIW, untyped, gp)
        case 32 => rshamt32(RV64Instr.SRAIW, untyped, gp)
        case _ => return None
      }
      case _ => return None
    }
    case OpcodeMap.OP => untyped.r.funct3 match {
      case 0 => untyped.r.funct7 match {
        case 0 => r(RV32Instr.ADD, untyped, gp)
        case 32 => r(RV32Instr.SUB, untyped, gp)
        case 1 => r(RV32Instr.MUL, untyped, gp)
        case _ => return None
      }
      case 1 => untyped.r.funct7 match {
        case 0 => r(RV32Instr.SLL, untyped, gp)
        case 1 => r(RV32Instr.MULH, untyped, gp)
        case _ => return None
      }
      case 2 => untyped.r.funct7 match {
        case 0 => r(RV32Instr.SLT, untyped, gp)
        case 1 => r(RV32Instr.MULHSU, untyped, gp)
        case _ => return None
      }
      case 3 => untyped.r.funct7 match {
        case 0 => r(RV32Instr.SLTU, untyped, gp)
        case 1 => r(RV32Instr.MULHU, untyped, gp)
        case _ => return None
      }
      case 4 => untyped.r.funct7 match {
        case 0 => r(RV32Instr.XOR, untyped, gp)
        case 1 => r(RV32Instr.DIV, untyped, gp)
        case _ => return None
      }
      case 5 => untyped.r.funct7 match {
        case 0 => r(RV32Instr.SRL, untyped, gp)
        case 32 => r(RV32Instr.SRA, untyped, gp)
        case 1 => r(RV32Instr.DIVU, untyped, gp)
        case _ => return None
      }
      case 6 => untyped.r.funct7 match {
        case 0 => r(RV32Instr.OR, untyped, gp)
        case 1 => r(RV32Instr.REM, untyped, gp)
        case _ => return None
      }
      case 7 => untyped.r.funct7 match {
        case 0 => r(RV32Instr.AND, untyped, gp)
        case 1 => r(RV32Instr.REMU, untyped, gp)
        case _ => return None
      }
    }
    case OpcodeMap.OP_32 => untyped.r.funct3 match {
      case 0 => untyped.r.funct7 match {
        case 0 => r(RV64Instr.ADDW, untyped, gp)
        case 32 => r(RV64Instr.SUBW, untyped, gp)
        case 1 => r(RV64Instr.MULW, untyped, gp)
      }
      case 1 => r(RV64Instr.SLLW, untyped, gp)
      case 4 => r(RV64Instr.DIVW, untyped, gp)
      case 5 => untyped.r.funct7 match {
        case 0 => r(RV64Instr.SRLW, untyped, gp)
        case 32 => r(RV64Instr.SRLW, untyped, gp)
        case 1 => r(RV64Instr.DIVUW, untyped, gp)
      }
      case 6 => r(RV64Instr.REMW, untyped, gp)
      case 7 => r(RV64Instr.REMUW, untyped, gp)
      case _ => return None
    }
    case OpcodeMap.MISC_MEM => untyped.i.funct3 match {
      case 0 => untyped.toU32.toLong match {
        case 0x8330000FL => RV32Instr.FENCE_TSO
        case 0x100000FL => RV32Instr.PAUSE
        case _ => fence(RV32Instr.FENCE, untyped, gp)
      }
      case 1 => i(RV64Instr.FENCE_I, untyped, gp)
      case _ => return None
    }
    case OpcodeMap.SYSTEM => untyped.i.funct3 match {
      case 0 => untyped.i.imm11_0 match {
        case 0 => RV32Instr.ECALL
        case 1 => RV32Instr.EBREAK
        case _ => untyped.r.funct7 match {
          case 8 => untyped.r.rs2 match {
            case 2 => RV64Instr.SRET
            case 5 => RV64Instr.WFI
            case _ => return None
          }
          case 24 => RV64Instr.MRET
          case 9 => r_no_rd(RV64Instr.SFENCE_VMA, untyped, gp)
          case 0xB => r_no_rd(RV64Instr.SINVAL_VMA, untyped, gp)
          case 0xC => untyped.r.rs2 match {
            case 0 => RV64Instr.SFENCE_W_INVAL
            case 1 => RV64Instr.SFENCE_INVAL_IR
            case _ => return None
          }
          case _ => throw new UnsupportedOperationException("Hypervisor")
        }
      }
      case 1 => zicsr_rs1(RV64Instr.CSRRW, untyped, gp)
      case 2 => zicsr_rs1(RV64Instr.CSRRS, untyped, gp)
      case 3 => zicsr_rs1(RV64Instr.CSRRC, untyped, gp)
      case 5 => zicsr_uimm(RV64Instr.CSRRWI, untyped, gp)
      case 6 => zicsr_uimm(RV64Instr.CSRRSI, untyped, gp)
      case 7 => zicsr_uimm(RV64Instr.CSRRCI, untyped, gp)
      case _ => return None
    }
    case OpcodeMap.AMO => untyped.ra.funct3 match {
      case 2 => untyped.ra.funct5 match {
        case 2 => ra_only_rs1(RV32Instr.LR_W, untyped, gp)
        case 3 => ra(RV32Instr.SC_W, untyped, gp)
        case 1 => ra(RV32Instr.AMOSWAP_W, untyped, gp)
        case 0 => ra(RV32Instr.AMOADD_W, untyped, gp)
        case 4 => ra(RV32Instr.AMOXOR_W, untyped, gp)
        case 12 => ra(RV32Instr.AMOAND_W, untyped, gp)
        case 8 => ra(RV32Instr.AMOOR_W, untyped, gp)
        case 16 => ra(RV32Instr.AMOMIN_W, untyped, gp)
        case 20 => ra(RV32Instr.AMOMAX_W, untyped, gp)
        case 24 => ra(RV32Instr.AMOMINU_W, untyped, gp)
        case 28 => ra(RV32Instr.AMOMAXU_W, untyped, gp)
        case _ => return None
      }
      case 3 => untyped.ra.funct5 match {
        case 2 => ra_only_rs1(RV64Instr.LR_D, untyped, gp)
        case 3 => ra(RV64Instr.SC_D, untyped, gp)
        case 1 => ra(RV64Instr.AMOSWAP_D, untyped, gp)
        case 0 => ra(RV64Instr.AMOADD_D, untyped, gp)
        case 4 => ra(RV64Instr.AMOXOR_D, untyped, gp)
        case 12 => ra(RV64Instr.AMOAND_D, untyped, gp)
        case 8 => ra(RV64Instr.AMOOR_D, untyped, gp)
        case 16 => ra(RV64Instr.AMOMIN_D, untyped, gp)
        case 20 => ra(RV64Instr.AMOMAX_D, untyped, gp)
        case 24 => ra(RV64Instr.AMOMINU_D, untyped, gp)
        case 28 => ra(RV64Instr.AMOMAXU_D, untyped, gp)
        case _ => return None
      }
      case _ => return None
    }
    case OpcodeMap._custom_0 |
         OpcodeMap._custom_1 |
         OpcodeMap._custom_2_or_rv128 |
         OpcodeMap._custom_3_or_rv128 |
         OpcodeMap._reversed_0 |
         OpcodeMap._reversed_1 |
         OpcodeMap._reversed_2 => return None
    case _ => return None
  })
}