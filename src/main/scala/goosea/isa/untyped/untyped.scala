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