package lars.strat.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.strat.DepGraph

/**
 * strongly connected components
 *
 * Created by hb on 7/10/15.
 */
object SCCs extends (DepGraph => Map[ExtendedAtom,DepGraph]) {


  //Kosaraju's Algorithm:
  // Let G be a directed graph and S be an empty stack.
  // While S does not contain all vertices:
  //   Choose an arbitrary vertex v not in S. Perform a depth-first search starting at v.
  //   Each time that depth-first search finishes expanding a vertex u, push u onto S.
  // Reverse the directions of all arcs to obtain the transpose graph.
  // While S is nonempty:
  //   Pop the top vertex v from S.
  //   Perform a depth-first search starting at v in the transpose graph.
  //   The set of visited vertices will give the strongly connected component containing v;
  //   record this and remove all these vertices from the graph G and the stack S.
  override def apply(G: DepGraph): Map[ExtendedAtom, DepGraph] = {
    val s = new collection.mutable.Stack[ExtendedAtom]()

    def dfsPush(g: DepGraph, n: ExtendedAtom) : Unit = {
      if (s.size != g.nodes.size) {
        if (!s.contains(n)) {
          s.push(n)
        }
        for (u <- g.neighbours(n)) {
          dfsPush(g, u)
        }
      }
    }
    dfsPush(G,G.nodes().head)

    val R = G.reverse()
    val visited = new collection.mutable.HashSet[ExtendedAtom]()

    def dfs(g: DepGraph, n: ExtendedAtom) : Unit = {
      visited += n
      for (u <- g.neighbours(n)) {
        if (!visited.contains(u)) {
          dfs(g, u)
        }
      }
    }

    val mMap = new collection.mutable.HashMap[ExtendedAtom,DepGraph]()
    var remG = R
    while (!s.isEmpty) {
      val v = s.pop()
      dfs(remG,v)
      val vis = visited.toSet
      mMap += v -> G.subGraph(vis)
      remG = remG -- vis
      visited.clear
    }

    mMap.toMap
  }



}
