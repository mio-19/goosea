package goosea

implicit class Fin32(i: Int) {
  if (!(0<=i && i<32)) {
    throw new IllegalArgumentException()
  }
}

implicit class U32(i: Int) {
  // todo
}

sealed trait Reg
object Reg {
  final case class X(f: Fin32) extends Reg
  final case class F(f:Fin32) extends Reg
  case object PC extends Reg
  case object CSR extends Reg
  case object FCSR extends Reg
}


sealed trait RoundingMode
object RoundingMode {
  // Round to nearest, ties to even
  case object RNE extends RoundingMode
  // Round towards zero
  case object RTZ extends RoundingMode
  // Round towards -infinity
  case object RDN extends RoundingMode
  // Round towards +infinity
  case object RUP extends RoundingMode
  // Round to nearest, ties to max magnitude
  case object RMM extends RoundingMode
  // In instruction's rm field, select dynamic rounding mode;
  // In Rounding Mode register, reserved.
  case object DYN extends RoundingMode
}

// This is used to represent lazily-decoded immediate value
// which is written as `imm[HIGH_BIT:LOW_BIT]` in the risc-v specification.
sealed abstract class Imm32(i: U32)
final case class Imm32_11_0(i: U32) extends Imm32(i)

sealed trait RV64Instr
