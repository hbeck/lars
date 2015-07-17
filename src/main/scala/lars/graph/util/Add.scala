package lars.graph.util

/**
 * Created by hb on 7/16/15.
 */
object Add {
  
  def apply[T](adjList:Map[T,Set[T]], from: T, to: T): Map[T,Set[T]] = {
    if (adjList.contains(from)) {
      val set: Set[T] = adjList(from) + to
      adjList.updated(from,set)
    } else {
      adjList + (from -> Set(to))
    }
  }

}
