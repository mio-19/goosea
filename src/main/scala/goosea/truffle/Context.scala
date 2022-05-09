package goosea.truffle

import goosea.cpu._
import goosea.utils._
import goosea.utils.num._
import goosea.mem._
import com.oracle.truffle.api.TruffleLanguage.ContextReference

final case class Context(cpu: RV64CPU = RV64CPU(),
                         compiled: ConcurrentCache[U64, GooseaNode] = ConcurrentCache()
                        ) {
}

object Context {
  def apply(): Context = Context()

  val REFERENCE: ContextReference[Context] = ContextReference.create(classOf[GooseaLang])

  def get(node: GooseaAbstractNode): Context = REFERENCE.get(node)
  def get(node: GooseaRootNode): Context = REFERENCE.get(node)
}