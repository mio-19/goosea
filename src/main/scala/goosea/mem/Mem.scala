package goosea.mem

import goosea.utils.num._

import java.nio.ByteBuffer
import java.io.ByteArrayInputStream


// 4MB
val pageSize: Int = 4 * 1024 * 1024 // 2^22
val pageAddr: Int = 22
val pageMask: Int = (1 << pageAddr) - 1

// thread-safe
// addr is unsigned Long
final class RawMem(pageNumber: Int) {
  val buff: Array[ByteBuffer] = new Array[ByteBuffer](pageNumber)

  def get(addr: Long): Byte = {
    val pageId: Int = (addr >> pageAddr).toInt
    val page = buff(pageId)
    if (page == null) 0 else page.synchronized {
      page.get((addr & pageMask).toInt)
    }
  }

  def getShort(addr: Long): Short = {
    if (addr >> pageAddr != (addr + 1) >> pageAddr) throw new IllegalArgumentException("unaligned address")
    val pageId: Int = (addr >> pageAddr).toInt
    val page = buff(pageId)
    if (page == null) 0.toShort else page.synchronized {
      page.getShort((addr & pageMask).toInt)
    }
  }

  def getInt(addr: Long): Int = {
    if (addr >> pageAddr != (addr + 3) >> pageAddr) throw new IllegalArgumentException("unaligned address")
    val pageId: Int = (addr >> pageAddr).toInt
    val page = buff(pageId)
    if (page == null) 0 else page.synchronized {
      page.getInt((addr & pageMask).toInt)
    }
  }

  def getLong(addr: Long): Long = {
    if (addr >> pageAddr != (addr + 7) >> pageAddr) throw new IllegalArgumentException("unaligned address")
    val pageId: Int = (addr >> pageAddr).toInt
    val page = buff(pageId)
    if (page == null) 0L else page.synchronized {
      page.getLong((addr & pageMask).toInt)
    }
  }

  def put(addr: Long, x: Byte) = {
    val pageId: Int = (addr >> pageAddr).toInt
    var page = buff(pageId)
    if (page == null) {
      buff.synchronized {
        page = buff(pageId)
        if (page == null) {
          page = ByteBuffer.allocate(pageSize)
          buff(pageId) = page
        }
      }
    }
    page.synchronized {
      page.put((addr & pageMask).toInt, x)
    }
  }

  // TODO: optimize me
  def load(addr: Long, xs: Array[Byte]): Unit = {
    for (i <- xs.indices) {
      this.put(addr + i, xs(i))
    }
  }

  def load(addr: Long, xs: ByteArrayInputStream): Unit = {
    var i = addr
    var data: Int = xs.read()
    while (data != -1) {
      this.put(i, data.toByte)
      i += 1
      data = xs.read()
    }
  }
}

final case class Mem(base: U64, size: U64, memory: RawMem) {
  def toPhys(addr: U64): U64 = {
    if (addr >= base && addr < base + size) {
      addr - base
    } else {
      throw new Exception("invalid address")
    }
  }

  def read8(addr: U64): U8 = U8(memory.get(toPhys(addr).toLong))

  def read16(addr: U64): U16 = U16(memory.getShort(toPhys(addr).toLong))

  def read32(addr: U64): U32 = U32(memory.getInt(toPhys(addr).toLong))

  def read64(addr: U64): U64 = U64(memory.getLong(toPhys(addr).toLong))
}

object Mem {
  def apply(base: U64, size: U64): Mem = {
    val (basePages, more): (U64, U64) = size /% pageSize
    val pages = if (more == 0) basePages else basePages + 1
    val memory = new RawMem(pages.toInt)
    new Mem(base, size, memory)
  }
}