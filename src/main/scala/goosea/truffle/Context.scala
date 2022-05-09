package goosea.truffle

import goosea.cpu._
import goosea.utils._
import goosea.utils.num._
import goosea.mem._
import com.oracle.truffle.api.TruffleLanguage.ContextReference

final case class Context(lang: GooseaLang,
                         cpu: RV64CPU = RV64CPU(),
                         compiled: ConcurrentCache[U64, GooseaNode] = ConcurrentCache()
                        ) {
  def getNode(pc: U64): GooseaNode = compiled.getOrElseUpdate(pc, new GooseaNode(pc))

  def getRootNode(pc: U64) = new GooseaRootNode(lang, getNode(pc))

  def tick(): Unit = getRootNode(cpu.readPC).createDirectCallNode().call()
}

object Context {
  def apply(lang: GooseaLang): Context = Context(lang)

  val REFERENCE: ContextReference[Context] = ContextReference.create(classOf[GooseaLang])

  def get(node: GooseaAbstractNode): Context = REFERENCE.get(node)

  def get(node: GooseaRootNode): Context = REFERENCE.get(node)
}