package lars.graph

import lars.graph.traits.{Outgoing, AdjList}

/**
 * graph based on adjacency list
 *
 * Created by hb on 7/16/15.
 */
class DiGraph[V](val adjList:Map[V,Set[V]]) extends AdjList[V] with Outgoing[V] {

  override def outgoing(n: V) = adjList(n)

  override def hasEdge(n:V, m:V) : Boolean = {
    outgoing(n).contains(m)
  }

  def isLeaf(n: V) = outgoing(n).isEmpty

  def subgraph(vertices: Set[V]): DiGraph[V] = {
    val keysOk: Map[V, Set[V]] = adjList.filterKeys( k => vertices.contains(k) )
    val valuesOk: Map[V, Set[V]] = keysOk.map( e => (e._1,e._2.filter( v => vertices.contains(v) )) )
    new DiGraph(valuesOk)
  }

  def ==(other: DiGraph[V]): Boolean = {
    adjList == other.adjList
  }

  def !=(other: DiGraph[V]): Boolean = {
    !(this == other)
  }

  override def equals(other:Any) : Boolean = {
    other match {
      case g: DiGraph[V] => this == g
      case _ => false
    }
  }

}
