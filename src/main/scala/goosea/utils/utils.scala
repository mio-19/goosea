package goosea.utils

import org.joou.*
import scodec.bits.BitVector
import org.joou.Unsigned.*

type U64 = ULong

object U64 {
  def apply(x: Int): U64 = ulong(x)
}

implicit def int2I64(x: Int): U64 = U64(x)

implicit class Fin32(i: Int) {
  if (!(0 <= i && i < 32)) {
    throw new IllegalArgumentException()
  }

  def toInt: Int = i
}

implicit def fin32ToInt(x: Fin32): Int = x.toInt

implicit class Fin16(i: Int) {
  if (!(0 <= i && i < 16)) {
    throw new IllegalArgumentException()
  }
}

type U16 = UShort

implicit def intToU16(x: Int): U16 = ushort(x)
implicit def u16ToInt(x: U16): Int = x.intValue
implicit def u16ToBitVector(x: U16): BitVector = BitVector.fromInt(x, 16)

type U32 = UInteger

object U32 {
  def apply(x: Int): U32 = uint(x)

  def apply(x: Long): U32 = uint(x)
}

implicit class U32Ops(x: U32) {
  def toInt: Int = x.intValue

  def toLong: Long = x.longValue

  def |(other: U32): U32 = this.toLong | U32Ops(other).toLong

  def <<(other: U32): U32 = this.toLong << U32Ops(other).toLong
  def >>(other: U32): U32 = this.toLong >> U32Ops(other).toLong

  def &(other: U32): U32 = this.toLong & U32Ops(other).toLong

  def toBitVector: BitVector = BitVector.fromLong(this.toLong, 32)
}

implicit def intToU32(x: Int): U32 = U32(x)
implicit def longToU32(x: Long): U32 = U32(x)

implicit class U8(i: Int) {
  if (!(0 <= i && i < 256)) {
    throw new IllegalArgumentException()
  }
}
