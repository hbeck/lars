package lars.util.map

import scala.collection.immutable.HashMap

/**
 * Created by hb on 7/16/15.
 */
object Reverse {

  //assume implicit bimap
  def apply[K,V](map: Map[K, V]): Map[V, K] = {
    var m = HashMap[V,K]()
    for ((k,v) <- map) {
      m += v -> k
    }
    m
  }

}
