package goosea.mem

import goosea.utils._

import java.nio.ByteBuffer
import java.io.ByteArrayInputStream


// 1MB
val pageSize: Int = 1024 * 1024 // 2^20
val pageAddr: Int = 20
val pageMask: Int = (1 << pageAddr) - 1

// 1GB
// thread-safe
final class RawMem(pageNumber: Int = 1024) {
  val buff: Array[ByteBuffer] = new Array[ByteBuffer](pageNumber)

  def get(addr: Int): Byte = {
    val page = buff(addr >> pageAddr)
    if (page == null) 0 else page.synchronized {
      page.get(addr & pageMask)
    }
  }

  def put(addr: Int, x: Byte) = {
    var page = buff(addr >> pageAddr)
    if (page == null) {
      buff.synchronized {
        page = buff(addr >> pageAddr)
        if (page == null) {
          page = ByteBuffer.allocate(pageSize)
          buff(addr >> pageAddr) = page
        }
      }
    }
    page.synchronized {
      page.put(addr & pageMask, x)
    }
  }

  // TODO: optimize me
  def load(addr: Int, xs: Array[Byte]): Unit = {
    for (i <- xs.indices) {
      this.put(addr + i, xs(i))
    }
  }

  def load(addr: Int, xs: ByteArrayInputStream): Unit = {
    var i = addr
    var data: Int = xs.read()
    while (data != -1) {
      this.put(i, data.toByte)
      i += 1
      data = xs.read()
    }
  }
}

final case class Mem(base: U64, size: U64, memory: RawMem)
object Mem {
  def apply(base: U64, size: U64): Mem = {
    val (basePages, more): (U64, U64) = size /% pageSize
    val pages = if(more.isZero) basePages else basePages + 1
    val memory = new RawMem(pages.toInt)
    new Mem(base, size, memory)
  }
}