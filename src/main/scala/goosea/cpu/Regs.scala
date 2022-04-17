package goosea.cpu

import goosea.utils._
import goosea.isa._

final class Regs(var x: Array[U64], var csr: U64, var pc: U64) {
  if (x.length != 32) throw new IllegalArgumentException()

  def read(reg: Reg): Option[U64] = Some(reg match {
    case Reg.X(id) => x(id)
    case Reg.CSR => csr
    case Reg.PC => pc
    case _ => return None
  })

  def write(reg: Reg, value: U64): Option[Unit] = {
    reg match {
      case Reg.X(id) => x(id) = value
      case Reg.CSR => csr = value
      case Reg.PC => pc = value
      case _ => return None
    }
    Some(())
  }
}

object Regs {
  def apply(pc: U64): Regs = new Regs(new Array[U64](32), 0, pc)
  def apply():Regs = Regs(0)
}
