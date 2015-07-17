package lars.graph.traits

/**
 * Created by hb on 7/17/15.
 */
trait AdjList[V] {

  def adjList:Map[V,Set[V]]

  def nodes:Set[V] = adjList.keySet

  def edges:Set[(V,V)] = {
    var set = Set[(V,V)]()
    for ((k,vs) <- adjList) {
      for (v <- vs) {
        set = set + ((k,v))
      }
    }
    set
  }
}
