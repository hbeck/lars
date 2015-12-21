package lars.graph.alg

import lars.graph.DiGraph
import lars.graph.quotient.Block

import scala.collection.immutable.HashMap

/**
 * strongly connected components partition function
 *
 * Created by hb on 7/10/15.
 *
 */
case class SCCFn[V]() extends (DiGraph[V] => Map[V,Block[V]]) {

  override def apply(g: DiGraph[V]): Map[V,Block[V]] = {
    val comps = BruteSCC[V](g)
    var componentM = HashMap[Int, Set[V]]()
    for (id <- comps.compId.values) {
      componentM = componentM + (id -> Set[V]())
    }
    for (n <- g.nodes) {
      val id = comps.compId(n)
      val set = componentM(id) + n
      componentM = componentM + (id -> set)
    }
    var map = HashMap[V, Block[V]]()
    for (component <- componentM.values) {
      for (n <- component) {
        map = map.updated(n, Block(component))
      }
    }
    map
  }
}
