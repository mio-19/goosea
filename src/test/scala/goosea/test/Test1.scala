package goosea.test

import goosea.cpu.Regs
import goosea.truffle.{Context, GooseaLang, GooseaNode, GooseaRootNode}
import goosea.utils.num.*

import scala.collection.mutable

val regs:Array[String] = Array(
  "$0", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
  "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
  "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
  "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6")

def Instrs(xs: U32*): Array[Byte] = {
  val bytes = new Array[Byte](xs.length * 4)
  var i = 0
  for (x <- xs) {
    bytes(i) = (x.toInt >> 24).toByte
    bytes(i + 1) = (x.toInt >> 16).toByte
    bytes(i + 2) = (x.toInt >> 8).toByte
    bytes(i + 3) = (x.toInt).toByte
    i += 4
  }
  bytes
}


final case class Test(pc: U64, instrs: Array[Byte], expected: String) {
  def run: Unit = {
    val sb = new mutable.StringBuilder
    val cpu = new GooseaLang
    val root = new GooseaRootNode(cpu, new GooseaNode(pc))
    val context = Context.get(root)
    context.cpu.bus.mem.load(pc, instrs)
    root.createDirectCallNode().call()
  }
}

def regsToString(regs: Regs): String = {
  val sb = new mutable.StringBuilder(s"pc=0x${regs.pc.toString(16)}")
  for(i <- regs.gr.indices) {
    val data = regs.gr(i)
    if(data!=0) sb.append(s" ${i}:${regs.gr(i)}=0x${regs.gr(i).toString(16)}")
  }
  sb.toString()
}

class Test1 {
  val test1 = Test(0x80000000, Instrs(
    0x3e800093, // addi x1 , x0,   1000  /* x1  = 1000 0x3E8 */
    0x7d008113, // addi x2 , x1,   2000  /* x2  = 3000 0xBB8 */
    0xc1810193, // addi x3 , x2,  -1000  /* x3  = 2000 0x7D0 */
    0x83018213, // addi x4 , x3,  -2000  /* x4  = 0    0x000 */
    0x3e820293, // addi x5 , x4,   1000  /* x5  = 1000 0x3E8 */
  ),
    """pc=0x80000004 1:ra=0x3e8
      |pc=0x80000008 1:ra=0x3e8 2:sp=0xbb8
      |pc=0x8000000c 1:ra=0x3e8 2:sp=0xbb8 3:gp=0x7d0
      |pc=0x80000010 1:ra=0x3e8 2:sp=0xbb8 3:gp=0x7d0
      |pc=0x80000014 1:ra=0x3e8 2:sp=0xbb8 3:gp=0x7d0 5:t0=0x3e8""".stripMargin)

  def instrs: Array[Long] = Array(

  )
}
