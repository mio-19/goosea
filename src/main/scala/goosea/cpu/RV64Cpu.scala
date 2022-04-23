package goosea.cpu

import goosea.isa.{Instr, Reg}
import goosea.isa.compressed.*
import goosea.isa.untyped.*
import goosea.utils.*
import goosea.mem.*
import goosea.cpu.bus.*

final class RV64CPU(
                     regs: Regs = Regs(),
                     bus: Bus = Bus(),
                     journal: Journal = Journal(),
                     wfi: Boolean = false
                   ) {
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

  final case class Fetch(pc: U64, bytecode: Bytecode, compressed: Bytecode16)
  def fetch: Fetch = {
    val pc = readPC
    ???
  }
  def mockFetch(pc: U64): Fetch = {
    ???
  }
  final case class Decode(from: Either[Bytecode, Bytecode16], decoded: Instr)
  def decode(fetch: Fetch): Decode = {
    ???
  }
  def execute(pc: U64, instr: Instr, isCompressed: Boolean): Unit = {
    ???
  }

  def tick: Unit = {
    if (this.wfi) {
      return
    }
    val fetch = this.fetch
    val Decode(from, decoded) = this.decode(fetch)
    val isCompressed = from.isRight
    this.execute(fetch.pc, decoded, isCompressed)
  }
  def mockTick(pc: U64):Unit = {
    ???
  }
}

object RV64CPU {
  def apply(): RV64CPU = new RV64CPU()
}

