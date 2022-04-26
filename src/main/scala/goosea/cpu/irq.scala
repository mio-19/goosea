package goosea.cpu

import goosea.utils.num.U64

sealed trait IRQ

object IRQ {
  // Machine external IRQ
  case object MEI extends IRQ

  // Machine software IRQ
  case object MSI extends IRQ

  // Machine timer IRQ
  case object MTI extends IRQ

  // Supervisor external IRQ
  case object SEI extends IRQ

  // Supervisor software IRQ
  case object SSI extends IRQ

  // Supervisor timer IRQ
  case object STI extends IRQ
}

sealed abstract class CPUThrowable extends Exception("CPU Throwable", null, false, false)

object CPUThrowable {
  case object IllegalInstruction extends CPUThrowable

  final case class LoadAccessFault(addr: U64) extends CPUThrowable

  final case class StoreAccessFault(addr: U64) extends CPUThrowable

  final case class LoadAddressMisaligned(addr: U64) extends CPUThrowable

  final case class StoreAddressMisaligned(addr: U64) extends CPUThrowable

  final case class InstructionPageFault(addr: U64) extends CPUThrowable

  final case class LoadPageFault(addr: U64) extends CPUThrowable

  final case class StorePageFault(addr: U64) extends CPUThrowable

  case object UserEcall extends CPUThrowable

  case object SupervisorEcall extends CPUThrowable

  case object MachineEcall extends CPUThrowable

  case object Breakpoint extends CPUThrowable
}