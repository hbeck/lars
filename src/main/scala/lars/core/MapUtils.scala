package lars.core

import scala.language.implicitConversions

/**
 * Created by hb on 5/19/15.
 */
object MapUtils {

  //returns a map v->K for every value V in K's Iterable of values
  //assuming that each v is unique in all K's sets
  def reverse[K,V](map: Map[K, Iterable[V]]): Map[V, K] = {
    val mMap = new collection.mutable.HashMap[V,K]()
    for ((k,values) <- map) {
      for (v <- values) {
        mMap += v -> k
      }
    }
    mMap.toMap
  }

  //TODO revisit
  def merge[K,V](m1: Map[K,Set[V]], m2: Map[K,Set[V]]) = {
    val keys:Set[K] = m1.keySet.union(m2.keySet)
    def mergedSet(k:K) = {
      val v1: Set[V] = m1.applyOrElse(k,{k:K=>Set[V]()})
      val v2: Set[V] = m2.applyOrElse(k,{k:K=>Set[V]()})
      v1 union v2
    }
    var m = Map[K,Set[V]]()
    for (k <- keys) {
      val v = mergedSet(k)
      m = m + ((k,v))
    }
    m
  }

  //TODO extract all map functions with maps of type [X,Set[Y]]



}
