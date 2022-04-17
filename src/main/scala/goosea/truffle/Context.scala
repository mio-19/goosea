package goosea.truffle

import goosea.cpu.Regs
import goosea.utils.U64

final case class Context(regs: Regs) {
}

object Context {
  def apply(pc: U64): Context = Context(Regs(pc))

  def apply(): Context = Context(Regs())
}