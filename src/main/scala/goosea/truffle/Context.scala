package goosea.truffle

import goosea.cpu.Regs
import goosea.utils.U64
import goosea.mem._

final case class Context(regs: Regs, mem: Mem) {
}

object Context {
  def apply(pc: U64): Context = Context(Regs(pc), Mem())

  def apply(): Context = Context(Regs(), Mem())
}