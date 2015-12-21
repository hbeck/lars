package lars.strat

/**
 * Created by hb on 12/21/15.
 *
 * (not located in lars.graph.alg, since it is not generic;
 * uses explicit DepGraph structure and increases counter iff '>' edges is encountered)
 */
object TopDownNumbering {

  def apply[V](rootedDAG: DepGraph[V], min:Int=0): Map[V, Int] = {
    val g = rootedDAG
    _apply(g,g.root().get,min)
  }

  def _apply[V](g: DepGraph[V], node:V, min:Int): Map[V, Int] = {
    if (g.isLeaf(node)) {
      Map() + (node -> min)
    } else {
      val children = g.outgoing(node)
      val childrenMaps: Set[Map[V, Int]] = children.map( child => _apply(g,child,min) )
      val nrMap = childrenMaps.reduce(_ ++ _)
      //consider all children's number + the potential increase due to '>',
      //and take the maximal resulting number
      val nodeNr = children.map{ child => nrMap(child) + addForEdge(g,node,child) }.max
      nrMap + (node -> nodeNr)
    }
  }

  //only encountering a greater edge ('>') increases the counter
  def addForEdge[V](g:DepGraph[V], from:V, to:V):Int = {
    val label: Dependency = g.label(from, to)
    label match {
      case `grt` => 1
      case _ => 0
    }
  }

}
