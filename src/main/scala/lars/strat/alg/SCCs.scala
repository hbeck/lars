package lars.strat.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.strat.DepGraph

/**
 * strongly connected components
 *
 * Created by hb on 7/10/15.
 *
 */
object SCCs extends (DepGraph => Map[ExtendedAtom,DepGraph]) {

  override def apply(G: DepGraph): Map[ExtendedAtom, DepGraph] = {
    val ds = BruteSCC(G)
    val componentM = new collection.mutable.HashMap[Int,Set[ExtendedAtom]]()
    for (id <- 0 to ds.count) {
      componentM(id)=Set[ExtendedAtom]()
    }
    for (n <- G.nodes) {
      val id = ds.compId(n)
      val set = componentM(id) + n
      componentM(id)=set
    }
    // rest of stratify alg was written before,
    // thus some further mappings. in principle,
    // we could already use the component mapping
    val components: Iterable[DepGraph] = componentM.values.map(G.subGraph)
    val mMap = new collection.mutable.HashMap[ExtendedAtom,DepGraph]()
    for (component <- components) {
      for (n <- component.nodes) {
        mMap(n)=component
      }
    }
    mMap.toMap
  }



}
