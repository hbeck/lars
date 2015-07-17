package lars.graph.alg

import lars.graph.DiGraph

import scala.collection.immutable.HashMap

/**
 * strongly connected components
 *
 * Created by hb on 7/10/15.
 *
 */
object SCCs {

  def apply[V](g: DiGraph[V]): Map[V, DiGraph[V]] = {
    val comps = BruteSCC[V](g)
    val componentM = new collection.mutable.HashMap[Int,Set[V]]()
    for (id <- comps.compId.values) {
      componentM(id)=Set[V]()
    }
    for (n <- g.nodes) {
      val id = comps.compId(n)
      val set = componentM(id) + n
      componentM(id)=set
    }
    // rest of stratify alg was written before,
    // thus some further mappings. in principle,
    // we could already use the component mapping
    val components: Iterable[DiGraph[V]] = componentM.values.map(g.subgraph)
    var map = HashMap[V,DiGraph[V]]()
    for (component <- components) {
      for (n <- component.nodes) {
        map = map.updated(n,component)
      }
    }
    map
  }

}
