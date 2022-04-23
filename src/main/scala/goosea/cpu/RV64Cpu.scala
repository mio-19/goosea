package goosea.cpu

import goosea.isa.{Instr, Reg}
import goosea.isa.compressed.*
import goosea.isa.untyped.*
import goosea.utils.*
import goosea.mem.*
import goosea.cpu.bus.*

/* VM modes (satp.mode) privileged ISA V20211203 */
val VM_V20211203_MBARE = 0
val VM_V20211203_SV39 = 8
val VM_V20211203_SV48 = 9
val VM_V20211203_SV57 = 10
val VM_V20211203_SV64 = 11

type VMMode = U8
object VMMode {
  val MBARE:VMMode = VM_V20211203_MBARE
  val SV39:VMMode = VM_V20211203_SV39
  val SV48:VMMode = VM_V20211203_SV48
  val SV57:VMMode = VM_V20211203_SV57
  val SV64:VMMode = VM_V20211203_SV64
}

final class RV64CPU(
                     regs: Regs = Regs(),
                     bus: Bus = Bus(),
                     journal: Journal = Journal(),
                     // used by wfi instruction
                     wfi: Boolean = false,
                     // Virtual memory translation mode
                     vmmode: VMMode = VMMode.MBARE,
                     // physical page number used in virtual memory translation
                     vmppn: U64 = 0,
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

  sealed trait Reason
  object Reason{
    case object Fetch extends Reason
    case object Read extends Reason
    case object Write extends Reason
  }
  def translate(addr: U64, reason: Reason): U64 ={
    if(this.vmmode == VMMode.MBARE){
      return addr
    }

    // 3.1.6.3 Memory Privilege in mstatus Register
    // The MPRV (Modify PRiVilege) bit modifies the effective privilege mode,
    // i.e., the privilege level at which loads and stores execute.
    // When MPRV=0, loads and stores behave as normal, using the translation and protection
    // mechanisms of the current privilege mode.
    // When MPRV=1, load and store memory addresses are translated and protected,
    // and endianness is applied, as though the current privilege mode were set to MPP.
    // Instruction address-translation and protection are unaffected by the setting of MPRV.
    // MPRV is read-only 0 if U-mode is not supported.
    //val eff_mode = reason match {
    //  case Reason.Fetch => this.mode,
    //}
    ???
  }
}

object RV64CPU {
  def apply(): RV64CPU = new RV64CPU()
}

