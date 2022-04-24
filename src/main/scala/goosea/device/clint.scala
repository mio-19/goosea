package goosea.device

import goosea.cpu.bus.CLINT_BASE
import goosea.utils.num._

package object clint {
  // machine timer register
  val MTIME = CLINT_BASE + 0xbff8
  val MTIME_END = MTIME + 8

  // machine timer compare register
  // 3.2.1 Machine Timer Registers (mtime and mtimecmp)
  // Lower privilege levels do not have their own timecmp registers.
  // Instead, machine-mode software can implement any number of virtual timers on a hart by multiplexing the next timer interrupt into the mtimecmp register.
  val MTIMECMP = CLINT_BASE + 0x4000
  val MTIMECMP_END = MTIMECMP + 8

  // machine software interrupt pending register
  val MSIP = CLINT_BASE
  val MSIP_END = MSIP + 4


  case class Clint(
                    msip: U32 = 0,
                    mtimecmp:U64 = 0,
                    mtime:U64 = 0
                  ) {

  }
}
