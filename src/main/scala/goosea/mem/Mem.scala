package goosea.mem

import java.nio.ByteBuffer


// 1MB
val pageSize: Int = 1024 * 1024 // 2^20
val pageAddr: Int = 20
val pageMask: Int = (1 << pageAddr) - 1

// 1GB
// thread-safe
final class Mem(pageNumber: Int = 1024) {
  val buff: Array[ByteBuffer] = new Array[ByteBuffer](pageNumber)

  def get(addr: Int): Byte = {
    val page = buff(addr >> pageAddr)
    if (page == null) 0 else page.synchronized { page.get(addr & pageMask) }
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
}