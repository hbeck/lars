package lars.util.map

/**
 * Created by hb on 7/16/15.
 */
object ReverseFromSet {

  //returns a map v->K for every value V in K's Iterable of values
  //assuming that each v is unique in all K's sets
  def apply[K,V](map: Map[K, Iterable[V]]): Map[V, K] = {
    val mMap = new collection.mutable.HashMap[V,K]()
    for ((k,values) <- map) {
      for (v <- values) {
        mMap += v -> k
      }
    }
    mMap.toMap
  }


}
