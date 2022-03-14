package goosea.utils

import scodec.bits.BitVector


// todo - fix me
implicit class U64(i: Long)

object U64 {
  def apply(x: Int): U64 = U64(x)
}


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

implicit class U32(i: Long) {
  if (!(0 <= i && i < 4294967296L)) {
    throw new IllegalArgumentException()
  }

  def toLong: Long = i

  def |(other: U32): U32 = this.toLong | other.toLong

  def >>(other: U32): U32 = this.toLong >> other.toLong
}

implicit def intToU32(x: Int): U32 = U32(x.toLong)
implicit def u32ToLong(x: U32): Long = x.toLong
implicit def u32ToBitVector(x: U32): BitVector = BitVector.fromLong(x, 32)

implicit class U8(i: Int) {
  if (!(0 <= i && i < 256)) {
    throw new IllegalArgumentException()
  }
}
