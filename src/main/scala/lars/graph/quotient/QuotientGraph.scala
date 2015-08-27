package lars.graph.quotient

import lars.graph.DiGraph
import lars.graph.util.{Add, HasEdge}

/**
 *
 * A quotient graph Q of a graph G is a graph whose vertices are blocks of a partition
 * of the vertices of G and where block A is adjacent to block B if some vertex x in A
 * is adjacent to some vertex y in B with respect to the edge set of G.
 *
 *
 * https://en.wikipedia.org/wiki/Quotient_graph
 *
 * Created by hb on 7/16/15.
 */
class QuotientGraph[V](override val adjList:Map[Block[V],Set[Block[V]]]) extends DiGraph[Block[V]](adjList)

object QuotientGraph {

  /**
   *
     @param prtFn partition function that returns for a DiGraph a mapping from nodes to the block it occurs in
   * @return quotient graph induced by prtFn
   */
  def apply[V, T <: DiGraph[V]](G: T, prtFn: (T => Map[V,Block[V]])): QuotientGraph[V] = {
    val block: Map[V,Block[V]] = prtFn(G) //Map of prtFns
    val nodes: Set[Block[V]] = G.nodes.map(block) //Set of prtFns
    var adjList = Map[Block[V],Set[Block[V]]]() //map from elems of nodes to neighbouring blocks
    for (n <- nodes) {
      adjList = adjList + (n -> Set[Block[V]]())
    }
    for ((x,y) <- G.edges) {
      val A = block(x)
      val B = block(y)
      if (A != B && !HasEdge(adjList,A,B)) {
        adjList = Add(adjList,A,B)
      }
    }
    new QuotientGraph(adjList)
  }
}
