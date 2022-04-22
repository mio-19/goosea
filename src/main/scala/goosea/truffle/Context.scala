package goosea.truffle

import goosea.cpu.Regs
import goosea.utils._
import goosea.mem._

final case class Context(regs: Regs, mem: Mem, jit: ConcurrentCache[U64, GooseaNode]) {
}

object Context {
  def apply(pc: U64): Context = Context(Regs(pc), Mem(), ConcurrentCache())

  def apply(): Context = Context(Regs(), Mem(), ConcurrentCache())
}