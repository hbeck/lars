package lars.util.graph

/**
 * Graph with node of type V
 *
 * Created by hb on 7/16/15.
 */
abstract class Graph[V] {

  def edges:Set[(V,V)]
  def nodes:Set[V]

  def hasEdge(from:V, to:V) = edges.contains((from,to))

}
