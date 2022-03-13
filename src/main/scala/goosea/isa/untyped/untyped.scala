package goosea.isa.untyped

import goosea.isa.U32
import scodec._
import scodec.bits._
import scodec.codecs._

implicit class Bytecode(instr: U32)

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