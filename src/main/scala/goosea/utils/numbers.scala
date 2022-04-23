// TODO: optimize me
package goosea.utils

import org.joou.*
import scodec.bits.BitVector
import org.joou.Unsigned.*
import java.lang.Long
import scala.language.implicitConversions
import java.math.BigInteger

type U64 = ULong

implicit class U64Ops(self: U64) {
  // From ULong.java
  /*
    @Override
    public String toString() {
        if (value >= 0)
            return Long.toString(value);
        else
            return BigInteger.valueOf(value & Long.MAX_VALUE).add(MAX_VALUE_LONG).toString();
    }
  */
  def toBigInteger = {
    val value = self.longValue
    if (value >= 0)
      BigInteger.valueOf(value)
    else
      BigInteger.valueOf(value & Long.MAX_VALUE).add(ULong.MAX_VALUE_LONG)
  }

  def /(that: U64): U64 = ULong.valueOf(self.toBigInteger.divide(that.toBigInteger))

  def %(that: U64): U64 = ULong.valueOf(self.toBigInteger.mod(that.toBigInteger))

  def /%(that: U64): (U64, U64) = {
    val result = self.toBigInteger.divideAndRemainder(that.toBigInteger)
    (ULong.valueOf(result(0)), ULong.valueOf(result(1)))
  }

  def +(that: U64): U64 = self.add(that)

  def +(that: Int): U64 = self.add(that)

  def +(that: Long): U64 = self.add(that)

  def >>(that: Int): U64 = ULong.valueOf(self.toBigInteger.shiftLeft(that))

  def <<(that: Int): U64 = ULong.valueOf(self.toBigInteger.shiftRight(that))

  def isZero = self equals ulong(0)

  def toInt = self.intValue
}

object U64 {
  def apply(x: Int): U64 = ulong(x)

  def repr(x: Long): U64 = ulong(x)
}

implicit def int2U64(x: Int): U64 = U64(x)

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

  def ==(other: Int) = i == other
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

implicit def intToU32(x: Int): U32 = U32(x)
implicit def longToU32(x: Long): U32 = U32(x)

implicit class U32Ops(x: U32) {
  def toInt: Int = x.intValue

  def toLong: Long = x.longValue

  def -(other: U32): U32 = x.subtract(other)

  def |(other: U32): U32 = U32(this.toLong | U32Ops(other).toLong)

  def <<(other: U32): U32 = U32(this.toLong << U32Ops(other).toLong)

  def >>(other: U32): U32 = U32(this.toLong >> U32Ops(other).toLong)

  def &(other: U32): U32 = U32(this.toLong & U32Ops(other).toLong)

  def toBitVector: BitVector = BitVector.fromLong(this.toLong, 32)
}

implicit class U8(i: Int) {
  if (!(0 <= i && i < 256)) {
    throw new IllegalArgumentException()
  }
}
