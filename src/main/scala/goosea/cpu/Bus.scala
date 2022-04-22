package goosea.cpu

import goosea.mem.Mem
import goosea.utils._

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
case class Bus(mem: Mem = Mem(RV64_MEMORY_BASE, RV64_MEMORY_SIZE)) {
}
