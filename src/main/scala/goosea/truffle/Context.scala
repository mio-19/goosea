package goosea.truffle

import goosea.cpu.*
import goosea.utils.*
import goosea.utils.num.*
import goosea.mem.*
import com.oracle.truffle.api.TruffleLanguage.ContextReference
import com.oracle.truffle.api.nodes.Node

final case class Context(lang: GooseaLang,
                         cpu: RV64CPU = RV64CPU(),
                         compiled: ConcurrentCache[U64, GooseaNode] = ConcurrentCache()
                        ) {
  def getNode(pc: U64): GooseaNode = compiled.getOrElseUpdate(pc, new GooseaNode(pc))

  def getRootNode(pc: U64) = new GooseaRootNode(lang, getNode(pc))

  def tick(): Unit = getRootNode(cpu.readPC).createDirectCallNode().call()

  def writePC(pc: U64): Unit = cpu.writePC(pc)
}

object Context {
  def apply(lang: GooseaLang): Context = Context(lang)

  val REFERENCE: ContextReference[Context] = ContextReference.create(classOf[GooseaLang])

  def get(node: Node): Context = REFERENCE.get(node)
}