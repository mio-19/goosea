package goosea.isa.untyped

import goosea.isa._
import scodec.{Attempt, DecodeResult}

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

private def u[T](opcode:(Reg,Imm32_31_12)=>T, untyped: Bytecode, reg: Int=>Reg): T = {
  val ut = unwarp(utypeCodec.decode(untyped))
  opcode(reg(ut.rd), Imm32(ut.imm31_12))
}

private def unwarp[T](x: Attempt[DecodeResult[T]]): T = {
  val v = x.require
  if (v.remainder.isEmpty) v.value else throw new IllegalArgumentException()
}

implicit class BytecodeOpcode(bytecode: Bytecode) {
  def opcode: Int = unwarp(opcodepeekCodec.decode(bytecode)).opcode
}


def decode(untyped: Bytecode): Option[Instr] = {
  val opcode = untyped.opcode >> 2 // stripping away `inst[1:0]=11`
  Some(opcode match {
    case OpcodeMap.LUI => u(RV32Instr.LUI, untyped, gp)
    case _ => return None
  })
}