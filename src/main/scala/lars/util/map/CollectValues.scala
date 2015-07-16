package lars.util.map

import scala.annotation.tailrec

/**
 * Created by hb on 7/16/15.
 */
object CollectValues {
  
  def apply[K,V](it: Iterable[(K,V)]): Map[K,Set[V]] = {
    @tailrec
    def _apply[K,V](it: Iterable[(K,V)], prt: Map[K,Set[V]]): Map[K,Set[V]] = {
      if (it.isEmpty) {
        prt
      }
      val head: (K, V) = it.head
      val k = head._1
      val v = head._2
      val set = prt.getOrElse(k,Set()) + v
      _apply(it.tail, prt.updated(k,set))
    }
    _apply(it,Map[K,Set[V]]())
  }

}
