package goosea.cpu

import goosea.utils._
import goosea.isa._
import goosea.isa.compressed.*
import goosea.isa.untyped.*

sealed trait Trace
object Trace{
  sealed trait TraceReg extends Trace
  object TraceReg {
    case class Read(reg: Reg, value: U64) extends TraceReg
    case class Write(reg: Reg, value: U64) extends TraceReg
  }
  sealed trait TraceInstr extends Trace
  object TraceInstr {
    case class Fetched(addr: U64, bytecode: Bytecode, bytecode16: Bytecode16) extends TraceInstr
    case class Decoded(addr: U64, bytecode: Bytecode, instr: Instr) extends TraceInstr
    case class DecodedCompressed(addr: U64, bytecode16: Bytecode16, instr: Instr) extends TraceInstr
    case class PrepareExecute(addr: U64, instr: Instr) extends TraceInstr
    case class Executed(addr: U64, instr: Instr) extends TraceInstr
    case class ExecutedCompressed(addr: U64, instr: Instr) extends TraceInstr
  }
}

sealed trait Journal {
  def trace(t: =>Trace): Unit
}

final class JournalReal extends Journal {
  def trace(t: =>Trace): Unit = {
    // TODO
  }
}

case object JournalDisabled extends Journal {
  def trace(t: =>Trace): Unit = {}
}