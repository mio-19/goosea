package goosea.isa.untyped

import scodec._
import scodec.bits._
import scodec.codecs._

// TODO
case class RType(opcode: Int, rd: Int, funct3: Int, rs1: Int, rs2: Int, funct7: Int)
implicit val rtypeCodec: Codec[RType] = (("opcode" | uint(7)) :: ("rd" | uint(5)) :: ("funct3" | uint(3)) :: ("rs1" | uint(5))::("rs2"|uint(5)) :: ("funct7" | uint(5))).as[RType]

case class IType(opcode: Int, rd: Int, funct3: Int, rs1: Int, imm11_0: Int)
implicit val itypeCodec: Codec[IType] = (("opcode" | uint(7)) :: ("rd" | uint(5))::("funct3"|uint(3))::("rs1"|uint(5))::("imm11_0"|uint(12))).as[IType]