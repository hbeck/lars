package lars.util.graph


/**
 * the condensation of a strongly connected graph is the quotient graph where
 * the strongly connected components form the blocks of the partition.
 *
 * Created by hb on 7/10/15.
 */
case class Condensation[V](override val adjList:Map[V,Set[V]]) extends InOutGraph[V](adjList)

object Condensation {

  // connect two components fromC and toC with an arc (in this direction),
  // if there is an edge (from,to) in g, where
  // from is in SCC fromC and to is in a different SCC toC  //
  // alg:
  //   for all edges (from,to) in g:
  //     get SCCs fromC, toC
  //     if fromC == toC continue //dep in edge can be >= or =
  //     if there exists already an edge between fromC, toC continue
  //       create edge (fromC,toC)
  //
  // result: DAG, not necessarily (weakly) connected
  //TODO factor out sccs by a generic function towards quotient
  def apply[V,G <: Graph[V]](g: G, sccs: collection.immutable.Map[V,G]): Condensation[G] = {
    val nodes: Set[G] = sccs.values.toSet
    var adjList = Map[G,Set[G]]()
    for (n <- nodes) {
      adjList = adjList + (n -> Set[G]())
    }
    for (e <- g.edges) {
      val fromC = sccs(e._1)
      val toC = sccs(e._2)
      if (fromC != toC && !HasEdge(adjList,fromC,toC)) {
        adjList = Add(adjList,fromC,toC)
      }
    }
    new Condensation(adjList)
  }

}
