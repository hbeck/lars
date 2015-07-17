package lars.graph.util

/**
 * Created by hb on 7/16/15.
 */
object HasEdge {

  def apply[T](adjList: Map[T,Set[T]], from: T, to: T): Boolean = {
    adjList.contains(from) && adjList(from).contains(to)
  }

}
