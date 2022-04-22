package goosea.truffle

import goosea.cpu._
import goosea.utils._
import goosea.mem._

final case class Context(cpu: RV64CPU, compiled: ConcurrentCache[U64, GooseaNode]) {
}

object Context {
  def apply(): Context = Context(RV64CPU(), ConcurrentCache())
}