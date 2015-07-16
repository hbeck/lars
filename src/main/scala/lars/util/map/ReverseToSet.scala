package lars.util.map

/**
 * Created by hb on 7/16/15.
 */
object ReverseToSet {

  def apply[K,V](map: Map[K,V]): Map[V,Set[K]] = {
    val mMap = new collection.mutable.HashMap[V,Set[K]]()
    for ((k,v) <- map) {
      if (mMap.contains(v)) {
        mMap(v) = mMap(v) + k
      } else {
        mMap(v) = collection.immutable.Set(k)
      }
    }
    mMap.toMap
  }

}
