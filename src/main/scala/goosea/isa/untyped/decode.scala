package goosea.isa.untyped

import goosea.isa.{Imm32_31_12, *}

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
    case _ => return None
  })
}