package goosea.isa.compressed

import goosea.isa.*
import goosea.utils.num._
import scodec.*
import scodec.bits.*
import scodec.codecs.*
import goosea.isa.untyped._

implicit class Bytecode16(val repr: U16) {
  def opcode = this.peek.op
  def funct3 = this.peek.funct3
  def peek = unwarp(opcodePeek16Codec.decode(repr))
  def cr = unwarp(crtypeCodec.decode(repr))
  def ci = unwarp(citypeCodec.decode(repr))
  def css = unwarp(csstypeCodec.decode(repr))
  def ciw = unwarp(ciwtypeCodec.decode(repr))
  def cl = unwarp(cltypeCodec.decode(repr))
  def cs = unwarp(cstypeCodec.decode(repr))
  def ca = unwarp(catypeCodec.decode(repr))
  def cb = unwarp(cbtypeCodec.decode(repr))
  def cbi = unwarp(cbitypeCodec.decode(repr))
  def cj = unwarp(cjtypeCodec.decode(repr))
}

final case class CRType(op: Int, rs2: Int, rd_or_rs1: Int, funct4: Int)

implicit val crtypeCodec: Codec[CRType] = (("op"|uint(2))::("rs2"|uint(5))::("rd_or_rs1"|uint(5))::("funct4"|uint(4))).as[CRType]

final case class CIType(op: Int, imm_b5: Int, rd: Int, imm_b1: Int, funct3: Int)
implicit val citypeCodec: Codec[CIType] = (("op"|uint(2))::("imm_b5"|uint(5))::("rd"|uint(5))::("imm_b1"|uint(1))::("funct3"|uint(3))).as[CIType]

final case class CSSType(op: Int, rs2: Int, imm_b6: Int, funct3: Int)
implicit val csstypeCodec: Codec[CSSType] = (("op"|uint(2))::("rs2"|uint(5))::("imm_b6"|uint(6))::("funct3"|uint(3))).as[CSSType]

final case class CIWType(op: Int, rd: Int, imm_b8: Int, funct3: Int)
implicit val ciwtypeCodec: Codec[CIWType] = (("op"|uint(2))::("rd"|uint(3))::("imm_b8"|uint(8))::("funct3"|uint(3))).as[CIWType]

final case class CLType(op: Int, rd: Int, imm_b2: Int, rs1: Int, imm_b3: Int, funct3: Int)
implicit val cltypeCodec: Codec[CLType] = (("op"|uint(2))::("rd"|uint(3))::("imm_b2"|uint(2))::("rs1"|uint(3))::("imm_b3"|uint(3))::("funct3"|uint(3))).as[CLType]

final case class CSType(op: Int, rs2: Int, imm_b2: Int, rs1: Int, imm_b3: Int, funct3: Int)
implicit val cstypeCodec:Codec[CSType]=(("op"|uint(2))::("rs2"|uint(3))::("imm_b2"|uint(2))::("rs1"|uint(3))::("imm_b3"|uint(3))::("funct3"|uint(3))).as[CSType]

final case class CAType(op: Int, rs2: Int, funct2: Int, rd_or_rs1: Int, funct6: Int)
implicit val catypeCodec:Codec[CAType]=(("op"|uint(2))::("rs2"|uint(3))::("funct2"|uint(2))::("rd_or_rs1"|uint(3))::("funct6"|uint(6))).as[CAType]

final case class CBType(op:Int, imm5:Int, imm2_1:Int, imm7_6: Int, rd_or_rs1: Int, imm4_3: Int, imm8: Int, funct3: Int)
implicit val cbtypeCodec:Codec[CBType]=(("op"|uint(2))::("imm5"|uint(1))::("imm2_1"|uint(2))::("imm7_6"|uint(2))::("rd_or_rs1"|uint(3))::("imm4_3"|uint(2))::("imm8"|uint(1))::("funct3"|uint(3))).as[CBType]

final case class CBIType(op: Int, imm4_0: Int, rd_or_rs1: Int, funct2: Int, imm5: Int, funct3: Int)
implicit val cbitypeCodec:Codec[CBIType]=(("op"|uint(2))::("imm4_0"|uint(5))::("rd_or_rs1"|uint(3))::("funct2"|uint(2))::("imm5"|uint(1))::("funct3"|uint(3))).as[CBIType]

final case class CJType(op: Int, imm5: Int, imm3_1:Int,imm7:Int,imm6:Int,imm10:Int,imm9_8:Int,imm4:Int,imm11:Int,funct3:Int)
implicit val cjtypeCodec:Codec[CJType]=(("op"|uint(2))::("imm5"|uint(1))::("imm3_1"|uint(3))::("imm7"|uint(1))::("imm6"|uint(1))::("imm10"|uint(1))::("imm9_8"|uint(2))::("imm4"|uint(1))::("imm11"|uint(1))::("funct3"|uint(3))).as[CJType]

final case class OpcodePeek16(op:Int, dummy:Int, funct3:Int)
implicit val opcodePeek16Codec:Codec[OpcodePeek16]=(("op"|uint(2))::("dummy"|uint(11))::("funct3"|uint(3))).as[OpcodePeek16]