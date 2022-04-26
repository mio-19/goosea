package goosea.utils.num

import scodec.bits.BitVector

import java.lang.Integer
import java.lang.ArithmeticException
import java.math.BigInteger

implicit def int2u8(i: Int): U8 = U8.checked(i)

implicit def u162u8(i: U16): U8 = U8.checked(i)

final case class U8(x: Byte) {
  def toByte: Byte = x

  def toShort: Short = this.toInt.toShort

  def toInt: Int = java.lang.Byte.toUnsignedInt(x)

  def toLong: Long = java.lang.Byte.toUnsignedLong(x)

  def toU16: U16 = U16(this.toInt)

  def toU32: U32 = U32(this.toInt)

  def toU64: U64 = U64(this.toLong)

  override def toString = this.toU32.toString

  def unary_~ : U8 = U8(~x)

  def +(y: U8): U8 = U8(x + y.toByte)

  def -(y: U8): U8 = U8(x - y.toByte)

  def *(y: U8): U8 = U8(this.toInt * y.toInt)

  def /(y: U8): U8 = U8(this.toInt / y.toInt)

  def %(y: U8): U8 = U8(this.toInt % y.toInt)

  def &(y: U8): U8 = U8(x & y.toByte)

  def |(y: U8): U8 = U8(x | y.toByte)

  def ^(y: U8): U8 = U8(x ^ y.toByte)

  def <<(y: U8): U8 = U8(x << y.toInt)

  def >>(y: U8): U8 = U8(x >>> y.toInt)
}

object U8 {
  def checked(x: Byte): U8 = {
    if (x < 0) throw new ArithmeticException("Overflow")
    U8(x)
  }

  def checked(x: Int): U8 = {
    if (x < 0 || x > 255) throw new ArithmeticException("Overflow")
    U8(x.toByte)
  }

  def checked(x: U16): U8 = checked(x.toInt)

  def apply(x: Byte): U8 = new U8(x)

  def apply(x: Int): U8 = new U8(x.toByte)

  def apply(x: Long): U8 = new U8(x.toByte)
}

implicit def int2u16(i: Int): U16 = U16.checked(i)

implicit def u162int(i: U16): Int = i.toInt

implicit def u162bitvector(i: U16): BitVector = i.toBitVector

final case class U16(x: Short) {
  def toShort: Short = x

  def toInt: Int = java.lang.Short.toUnsignedInt(x)

  def toLong: Long = java.lang.Short.toUnsignedLong(x)

  def toU32: U32 = U32(this.toInt)

  def toU64: U64 = U64(this.toLong)

  def toBitVector: BitVector = BitVector.fromInt(this.toInt, 16)

  override def toString: String = this.toU32.toString

  def unary_~ : U16 = U16(~x)

  def +(y: U16): U16 = U16(x + y.toShort)

  def -(y: U16): U16 = U16(x - y.toShort)

  def *(y: U16): U16 = U16(this.toInt * y.toInt)

  def /(y: U16): U16 = U16(this.toInt / y.toInt)

  def %(y: U16): U16 = U16(this.toInt % y.toInt)

  def &(y: U16): U16 = U16(x & y.toShort)

  def |(y: U16): U16 = U16(x | y.toShort)

  def ^(y: U16): U16 = U16(x ^ y.toShort)

  def <<(y: U16): U16 = U16(x << y.toInt)

  def <<(y: Int): U16 = U16(x << y)

  def >>(y: U16): U16 = U16(x >>> y.toInt)

  def >>(y: Int): U16 = U16(x >>> y)

  def ==(y: U16): Boolean = x == y.toShort

  // undefined for negative numbers
  def ==(y: Int): Boolean = x == y

  def ==(y: Long): Boolean = x == y
}

object U16 {
  def apply(x: Int): U16 = U16(x.toShort)

  def checked(x: Int): U16 = if (0 <= x && x < 65536) {
    U16(x)
  } else {
    throw new ArithmeticException("input is out of range!")
  }
}

implicit def int2U32(x: Int): U32 = U32.checked(x)

implicit def long2U32(x: Long): U32 = U32.checked(x)

implicit def u162u32(x: U16): U32 = x.toU32

final case class U32(x: Int) {
  def toInt: Int = x

  def toLong: Long = Integer.toUnsignedLong(x)

  def toBitVector: BitVector = BitVector.fromLong(this.toLong, 32)

  def toU64: U64 = U64(this.toLong)

  override def toString: String = Integer.toUnsignedString(x)

  def unary_~ : U32 = U32(~x)

  def +(y: U32): U32 = U32(x + y.toInt)

  def +(y: Int): U32 = U32(x + y)

  def +(y: Long): U32 = U32(x + y.toInt)

  def -(y: U32): U32 = U32(x - y.toInt)

  def -(y: Int): U32 = U32(x - y)

  def -(y: Long): U32 = U32(x - y.toInt)

  def *(y: U32): U32 = U32(this.toLong * y.toLong)

  /// undefined when y is less than 0
  def *(y: Int): U32 = U32(this.toLong * y)

  def *(y: Long): U32 = U32(this.toLong * y)

  def /(y: U32): U32 = U32(Integer.divideUnsigned(x, y.toInt))

  def %(y: U32): U32 = U32(Integer.remainderUnsigned(x, y.toInt))

  def &(y: U32): U32 = U32(x & y.toInt)

  def |(y: U32): U32 = U32(x | y.toInt)

  def ^(y: U32): U32 = U32(x ^ y.toInt)

  def <<(y: U8): U32 = U32(x << y.toInt)

  def <<(y: U32): U32 = U32(x << y.toInt)

  def <<(y: U64): U32 = U32(x << y.toLong)

  def <<(y: Int): U32 = U32(x << y)

  def >>(y: U32): U32 = U32(x >>> y.toInt)

  def >>(y: U64): U32 = U32(x >>> y.toLong)

  def >>(y: Int): U32 = U32(x >>> y)

  def >>(y: U8): U32 = U32(x >>> y.toInt)

  // undefined for negative numbers
  def ==(y: U32): Boolean = x == y.toInt

  def ==(y: Int): Boolean = x == y
}

object U32 {
  def apply(x: Int): U32 = new U32(x)

  def apply(x: Long): U32 = new U32(x.toInt)

  def checked(x: Int): U32 = if (0 <= x) {
    U32(x)
  } else {
    throw new ArithmeticException("input is negative!")
  }

  def checked(x: Long): U32 = if (0 <= x && x <= 0xffffffffL) {
    U32(x)
  } else {
    throw new ArithmeticException("input is out of range!")
  }
}

implicit def int2U64(x: Int): U64 = U64.checked(x)

implicit def long2U64(x: Long): U64 = U64.checked(x)

final case class U64(x: Long) {
  def toU8: U8 = U8(x.toByte)

  def toU16: U16 = U16(x.toShort)

  def toU32: U32 = U32(x.toInt)

  def toLong: Long = x

  def toInt: Int = x.toInt

  def toBigInteger: BigInteger =
    if (x >= 0)
      BigInteger.valueOf(x)
    else
      BigInteger.valueOf(x & java.lang.Long.MAX_VALUE).add(U64.MAX_VALUE_LONG)

  override def toString: String = java.lang.Long.toUnsignedString(x)

  def toString(radix: Int): String = java.lang.Long.toUnsignedString(x, radix)

  def unary_~ : U64 = U64(~x)

  def +(y: U64): U64 = U64(x + y.toLong)

  def -(y: U64): U64 = U64(x - y.toLong)

  // TODO: optimize me
  def *(y: U64): U64 = U64(this.toBigInteger.multiply(y.toBigInteger))

  def /(y: U64): U64 = U64(java.lang.Long.divideUnsigned(x, y.toLong))

  def %(y: U64): U64 = U64(java.lang.Long.remainderUnsigned(x, y.toLong))

  def /%(y: U64): (U64, U64) = {
    val q = java.lang.Long.divideUnsigned(x, y.toLong)
    val r = java.lang.Long.remainderUnsigned(x, y.toLong)
    (U64(q), U64(r))
  }

  def &(y: U64): U64 = U64(x & y.toLong)

  def |(y: U64): U64 = U64(x | y.toLong)

  def ^(y: U64): U64 = U64(x ^ y.toLong)

  def <<(y: U64): U64 = U64(x << y.toInt)

  def <<(y: Int): U64 = U64(x << y)

  def <<(y: U8): U64 = U64(x << y.toInt)

  def >>(y: U64): U64 = U64(x >>> y.toInt)

  def >>(y: Int): U64 = U64(x >>> y)

  def >>(y: U8): U64 = U64(x >>> y.toInt)

  def compare(y: U64): Int = java.lang.Long.compareUnsigned(x, y.toLong)

  def >(y: U64): Boolean = this.compare(y) > 0

  def >=(y: U64): Boolean = this.compare(y) >= 0

  def <(y: U64): Boolean = this.compare(y) < 0

  def <=(y: U64): Boolean = this.compare(y) <= 0

  def ==(y: U64): Boolean = x == y.toLong

  // undefined for negative numbers
  def ==(y: Int): Boolean = x == y
}

object U64 {
  /**
   * A constant holding the maximum value + 1 an <code>signed long</code> can
   * have, 2<sup>63</sup>.
   */
  val MAX_VALUE_LONG: BigInteger = new BigInteger("9223372036854775808");

  val MaxValue = U64("18446744073709551615")

  def apply(x: Int): U64 = new U64(x)

  def apply(x: Long): U64 = new U64(x)

  def apply(x: BigInteger): U64 = new U64(x.longValue())

  def apply(x: String): U64 = U64(new BigInteger(x))

  def checked(x: Int): U64 = if (0 <= x) {
    U64(x)
  } else {
    throw new ArithmeticException("input is negative!")
  }

  def checked(x: Long): U64 = if (0 <= x) {
    U64(x)
  } else {
    throw new ArithmeticException("input is negative!")
  }
}

implicit def int2fin32(x: Int): Fin32 = Fin32.checked(x)

final case class Fin32(x: Byte) {
  def toInt: Int = x.toInt

  def toLong: Long = x.toLong
}

object Fin32 {
  def apply(x: Int): Fin32 = new Fin32((x & 0x1f).toByte)

  def apply(x: Byte): Fin32 = new Fin32((x & 0x1f).toByte)

  def checked(x: Int): Fin32 = {
    if (0 <= x && x <= 31)
      Fin32(x)
    else
      throw new ArithmeticException("input is out of range!")
  }
}

implicit def int2fin16(x: Int): Fin16 = Fin16.checked(x)

final case class Fin16(x: Byte) {
  def toInt: Int = x.toInt

  def toLong: Long = x.toLong
}

object Fin16 {
  def apply(x: Int): Fin16 = new Fin16((x & 0x0f).toByte)

  def apply(x: Byte): Fin16 = new Fin16((x & 0x0f).toByte)

  def checked(x: Int): Fin16 = {
    if (0 <= x && x <= 15)
      Fin16(x)
    else
      throw new ArithmeticException("input is out of range!")
  }
}