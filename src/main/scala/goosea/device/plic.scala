package goosea.device

import goosea.cpu.bus.PLIC_BASE

package object plic {
  val SOURCE_PRIORITY = PLIC_BASE
  val SOURCE_PRIORITY_END = PLIC_BASE + 0xfff

  val PENDING = PLIC_BASE + 0x1000
  val PENDING_END = PLIC_BASE + 0x107f

  val ENABLE = PLIC_BASE + 0x2000
  val ENABLE_END = PLIC_BASE + 0x20ff

  val THRESHOLD_AND_CLAIM = PLIC_BASE + 0x200000
  val THRESHOLD_AND_CLAIM_END = PLIC_BASE + 0x201007

  val WORD_SIZE = 0x4
  val CONTEXT_OFFSET = 0x1000
  val SOURCE_NUM = 1024


  // The platform-level-interrupt controller (PLIC).
  // TODO
  case class Plic()
}
