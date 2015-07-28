package lars.strat

import lars.core.semantics.formulas.ExtendedAtom
import lars.graph.alg.DepPartition
import lars.graph.quotient.QuotientGraph

/**
 * Created by et on 22.07.15.
 */
object StratumGraph {

  /*NOTE new quotientgraph from DepGraph*/
  def apply(g: DepGraph): QuotientGraph[ExtendedAtom]  = {
    //QuotientGraph(g,DepPartition())
    QuotientGraph(g,DepPartition())
  }

/*
  def isGrt(dep:DepGraph, from: ExtendedAtom, to: ExtendedAtom): Boolean = {
    g.label(from, to) match {
      case `grt` => true
      case _ => false
    }
  }

  /*NOTE don't make new quotientgraph, but merge vertices of existing
     * UPDATE i don't know how to read the dependencies from the quotient graph - it does not extend a DepGraph*/
    def apply[V](g: QuotientGraph[ExtendedAtom], dep: DepGraph): QuotientGraph[V] = {
    g.edges
    var r = new collection.mutable.HashMap[Set[V], Set[Set[V]]]()
    var min = false
      var res = g
      /*
      * @x Set of vertices
      * @y Set of set of vertices, which is adjacent to x
      */
      for ((x,y)<-g.adjList) {
        for(z <- y){
          for(w <- x){
            for(a <- z){
              if(dep.hasEdge(w,a)){
                if(isGrt(dep,w,a)){
                  /*do sth*/
                }
              }
            }
          }

  //        dfs(g,x,z,)
        }
      }

      res
    }*/

  /*    var r = new collection.mutable.HashMap[ExtendedAtom, Set[ExtendedAtom]]()
    var min = false
    // var s:Set[ExtendedAtom] = Set(g.nodes.head)
    for (v <- g.nodes) {
      r += (v -> Set())
      for (w <- g.adjList(v)) {
        if (!initDfs(g, v, w)) {
          //  s += w
          r(v) += w
        }
      }
    }
    r.toMap*/
}
