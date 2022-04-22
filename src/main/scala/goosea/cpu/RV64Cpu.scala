package goosea.cpu

import goosea.isa.Reg
import goosea.isa.compressed.*
import goosea.isa.untyped.*
import goosea.utils.*
import goosea.mem.*

final case class Fetch(pc: U64, bytecode: Bytecode, compressed: Bytecode16)

final class RV64CPU(regs: Regs = Regs(), mem: Mem = Mem(), journal: Journal = Journal()) {
  def readReg(reg: Reg): U64 = {
    val x = regs.read(reg)
    journal.trace(Trace.TraceReg.Read(reg, x))
    x
  }

  def writeReg(reg: Reg, value: U64): Unit = {
    regs.write(reg, value)
    journal.trace(Trace.TraceReg.Write(reg, value))
  }

  def readPC: U64 = readReg(Reg.PC)

  def writePC(pc: U64) = writeReg(Reg.PC, pc)

  // TODO
  def fetch: Fetch = ???
}

object RV64CPU {
  def apply(): RV64CPU = new RV64CPU()
}

