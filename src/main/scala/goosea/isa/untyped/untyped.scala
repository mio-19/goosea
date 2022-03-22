package goosea.isa.untyped

import goosea.isa.*
import goosea.utils._
import scodec.*
import scodec.bits.*
import scodec.codecs.*

import scala.language.implicitConversions

private def unwarp[T](x: Attempt[DecodeResult[T]]): T = {
  val v = x.require
  if (v.remainder.isEmpty) v.value else throw new IllegalArgumentException()
}

implicit class Bytecode(instr: U32) {
  def toU32: U32 = instr

  def opcode: Int = unwarp(opcodepeekCodec.decode(instr)).opcode

  def b = unwarp(btypeCodec.decode(instr))

  def i = unwarp(itypeCodec.decode(instr))

  def j = unwarp(jtypeCodec.decode(instr))

  def u = unwarp(utypeCodec.decode(instr))

  def s = unwarp(stypeCodec.decode(instr))

  def r = unwarp(rtypeCodec.decode(instr))

  def ra = unwarp(ratypeCodec.decode(instr))

  def rshamt64 = unwarp(rshamt64typeCodec.decode(instr))

  def rshamt32 = unwarp(rshamt32typeCodec.decode(instr))

  def fence = unwarp(fencetypeCodec.decode(instr))
}

implicit def bytecode2u32(x: Bytecode): U32 = x.toU32
implicit def bytecode2bitvector(x: Bytecode): BitVector = x

sealed trait UntypedInstr

// TODO
final case class RType(opcode: Int, rd: Int, funct3: Int, rs1: Int, rs2: Int, funct7: Int) extends UntypedInstr

implicit val rtypeCodec: Codec[RType] = (("opcode" | uint(7)) :: ("rd" | uint(5)) :: ("funct3" | uint(3)) :: ("rs1" | uint(5)) :: ("rs2" | uint(5)) :: ("funct7" | uint(5))).as[RType]

final case class IType(opcode: Int, rd: Int, funct3: Int, rs1: Int, imm11_0: Int) extends UntypedInstr

implicit val itypeCodec: Codec[IType] = (("opcode" | uint(7)) :: ("rd" | uint(5)) :: ("funct3" | uint(3)) :: ("rs1" | uint(5)) :: ("imm11_0" | uint(12))).as[IType]

final case class SType(opcode: Int, imm4_0: Int, funct3: Int, rs1: Int, rs2: Int, imm11_5: Int) extends UntypedInstr

implicit val stypeCodec: Codec[SType] = (("opcode" | uint(7)) :: ("imm4_0" | uint(5)) :: ("funct3" | uint(3)) :: ("rs1" | uint(5)) :: ("rs2" | uint(5)) :: ("imm11_5" | uint(7))).as[SType]

final case class BType(opcode: Int, imm11: Int, imm4_1: Int, funct3: Int, rs1: Int, rs2: Int, imm10_5: Int, imm12: Int) extends UntypedInstr

implicit val btypeCodec: Codec[BType] = (("opcode" | uint(7)) :: ("imm11" | uint(1)) :: ("imm4_1" | uint(4)) :: ("funct3" | uint(3)) :: ("rs1" | uint(5)) :: ("rs2" | uint(5)) :: ("imm10_5" | uint(6)) :: ("imm12" | uint(1))).as[BType]

final case class UType(opcode: Int, rd: Int, imm31_12: Int) extends UntypedInstr

implicit val utypeCodec: Codec[UType] = (("opcode" | uint(7)) :: ("rd" | uint(5)) :: ("imm31_12" | uint(20))).as[UType]

final case class JType(opcode: Int, rd: Int, imm19_12: Int, imm11: Int, imm10_1: Int, imm20: Int) extends UntypedInstr

implicit val jtypeCodec: Codec[JType] = (("opcode" | uint(7)) :: ("rd" | uint(5)) :: ("imm19_12" | uint(8)) :: ("imm11" | uint(1)) :: ("imm10_1" | uint(10)) :: ("imm20" | uint(1))).as[JType]

final case class R4Type(opcode: Int, rd: Int, funct3: Int, rs1: Int, rs2: Int, funct2: Int, rs3: Int) extends UntypedInstr

implicit val r4typeCodec: Codec[R4Type] = (("opcode" | uint(7)) :: ("rd" | uint(5)) :: ("funct3" | uint(3)) :: ("rs1" | uint(5)) :: ("rs2" | uint(5)) :: ("funct2" | uint(2)) :: ("rs3" | uint(5))).as[R4Type]

final case class RShamt32Type(opcode: Int, rd: Int, funct3: Int, rs1: Int, shamt: Int, funct7: Int) extends UntypedInstr

implicit val rshamt32typeCodec: Codec[RShamt32Type] = (("opcode" | uint(7)) :: ("rd" | uint(5)) :: ("funct3" | uint(3)) :: ("rs1" | uint(5)) :: ("shamt" | uint(5)) :: ("funct7" | uint(7))).as[RShamt32Type]

final case class RAType(opcode: Int, rd: Int, funct3: Int, rs1: Int, rs2: Int, rl: Boolean, aq: Boolean, funct5: Int) extends UntypedInstr

implicit val ratypeCodec: Codec[RAType] = (("opcode" | uint(7)) :: ("rd" | uint(5)) :: ("funct3" | uint(3)) :: ("rs1" | uint(5)) :: ("rs2" | uint(5)) :: ("rl" | bool) :: ("aq" | bool) :: ("funct5" | uint(5))).as[RAType]

final case class RShamt64Type(opcode: Int, rd: Int, funct3: Int, rs1: Int, shamt: Int, funct6: Int) extends UntypedInstr

implicit val rshamt64typeCodec: Codec[RShamt64Type] = (("opcode" | uint(7)) :: ("rd" | uint(5)) :: ("funct3" | uint(3)) :: ("rs1" | uint(5)) :: ("shamt" | uint(6)) :: ("funct6" | uint(6))).as[RShamt64Type]

final case class FenceType(opcode: Int, rd: Int, funct3: Int, rs1: Int, succ: Int, pred: Int, fm: Int) extends UntypedInstr

implicit val fencetypeCodec: Codec[FenceType] = (("opcode" | uint(7)) :: ("rd" | uint(5)) :: ("funct3" | uint(3)) :: ("rs1" | uint(5)) :: ("succ" | uint(4)) :: ("pred" | uint(4)) :: ("fm" | uint(4))).as[FenceType]

final case class OpcodePeek(opcode: Int, dummy: Int) extends UntypedInstr

implicit val opcodepeekCodec: Codec[OpcodePeek] = (("opcode" | uint(7)) :: ("dummy" | uint(25))).as[OpcodePeek]
