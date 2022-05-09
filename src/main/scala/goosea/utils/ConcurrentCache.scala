package goosea.utils

import scala.annotation.tailrec
import scala.ref.WeakReference
import scala.collection.concurrent.TrieMap

// TODO: use Int2ObjectMap
final class ConcurrentCache[K, V <: AnyRef] {
  private val cache = TrieMap[K, WeakReference[V]]()

  def get(key: K): Option[V] = cache.get(key) match {
    case Some(ref) => ref.get match {
      case Some(v) => Some(v)
      case None => {
        cache.remove(key, ref)
        None
      }
    }
    case None => None
  }

  @tailrec
  def getOrElseUpdate(key: K, value: => V): V = cache.get(key) match {
    case Some(ref) => ref.get match {
      case Some(v) => v
      case None => {
        val result = value
        if (cache.replace(key, ref, WeakReference(result))) {
          result
        } else {
          getOrElseUpdate(key, result)
        }
      }
    }
    case None => {
      val result = value
      if (cache.putIfAbsent(key, WeakReference(result)).isEmpty) {
        result
      } else {
        getOrElseUpdate(key, result)
      }
    }
  }

  def put(key: K, value: V): Unit = cache.put(key, WeakReference(value))
}

object ConcurrentCache {
  def apply[K, V <: AnyRef](): ConcurrentCache[K, V] = new ConcurrentCache[K, V]()
}