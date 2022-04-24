package goosea.cpu

import goosea.mem.Mem
import goosea.utils.num._
import goosea.device.*
import goosea.device.clint.Clint
import goosea.device.plic.Plic
import goosea.device.virtio.Virtio

import scala.collection.immutable.HashMap

package object bus {
  val RV64_MEMORY_BASE = 0x80000000
  val RV64_MEMORY_SIZE = 1024 * 1024 * 1024
  val RV64_MEMORY_END = RV64_MEMORY_BASE + RV64_MEMORY_SIZE

  val VIRT_MROM_BASE = 0x1000
  val VIRT_MROM_SIZE = 0xf000
  val VIRT_MROM_END = VIRT_MROM_BASE + VIRT_MROM_SIZE

  val CLINT_BASE = 0x2000000
  val CLINT_SIZE = 0x10000
  val CLINT_END = CLINT_BASE + CLINT_SIZE

  val PLIC_BASE = 0xc000000
  val PLIC_SIZE = 0x208000
  val PLIC_END = PLIC_BASE + PLIC_SIZE

  // The address which virtio starts.
  val VIRTIO_BASE = 0x10001000
  val VIRTIO_SIZE = 0x1000
  val VIRTIO_END = VIRTIO_BASE + VIRTIO_SIZE


  // TODO
  // System Bus, which handles DRAM access and memory-mapped IO.
  // https://github.com/qemu/qemu/blob/master/hw/riscv/virt.c
  // Builtin IO maps:
  // - 0x1000      - 0x1000 + 0xf000       ==== Virt_MROM, like device trees
  // - 0x0x2000000 - 0x2000000 + 0x10000   ==== CLINT
  case class Bus(
                  mem: Mem = Mem(RV64_MEMORY_BASE, RV64_MEMORY_SIZE),
                  devices: Seq[Device] = Seq(),
                  ioMap:HashMap[(U64, U64), U64] = HashMap(),

                  // Builtin IO devices
                  deviceTree: Mem = Mem(VIRT_MROM_BASE, VIRT_MROM_SIZE),
                  clint: Clint = Clint(),
                  plic: Plic = Plic(),
                  virtio: Virtio = Virtio()
                ) {

    private def read(addr: U64, size: Int): U8|U16|U32|U64 = {
      // fast-path for builtin io devices
      if(RV64_MEMORY_BASE<= addr && addr < RV64_MEMORY_END) {
        size match {
          case 1 => mem.read8(addr)
          case 2 => mem.read16(addr)
          case 4 => mem.read32(addr)
          case 8 => mem.read64(addr)
          case _ => throw new Exception(s"Unsupported size: $size")
        }
      } else if(VIRT_MROM_BASE <= addr && addr < VIRT_MROM_END) {
        size match {
          case 1 => deviceTree.read8(addr)
          case 2 => deviceTree.read16(addr)
          case 4 => deviceTree.read32(addr)
          case 8 => deviceTree.read64(addr)
          case _ => throw new Exception(s"Unsupported size: $size")
        }
      } else if(CLINT_BASE <= addr && addr < CLINT_END) {
        ???
      }
      ???
    }

    def read8(addr: U64): U8 = this.read(addr, 1).asInstanceOf[U8]
    def read16(addr: U64): U16 = this.read(addr, 2).asInstanceOf[U16]
    def read32(addr: U64): U32 = this.read(addr, 4).asInstanceOf[U32]
    def read64(addr: U64): U64 = this.read(addr, 8).asInstanceOf[U64]
  }

}
