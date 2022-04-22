package goosea.cpu

import goosea.utils._
import goosea.isa._

sealed trait Trace
object Trace{
  sealed trait TraceReg extends Trace
  object TraceReg {
    case class Read(reg: Reg, value: U64) extends TraceReg
    case class Write(reg: Reg, value: U64) extends TraceReg
  }
}

final class Journal {
  def trace(t: =>Trace): Unit = {
    // TODO
  }
}