package goosea.cpu

import goosea.utils._
import goosea.mem._

final class RV64CPU(regs: Regs, mem: Mem) {
  // TODO
}

object RV64CPU {
  def apply(): RV64CPU = new RV64CPU(Regs(), Mem())
}