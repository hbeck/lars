package lars.strat

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram
import lars.graph.quotient.{Block, Condensation, QuotientGraph}
import lars.util.map.ReverseFromSet

/**
 * Stream Stratification
 * Created by hb on 7/7/15.
 */
case class Stratification(map: Map[Int,Set[ExtendedAtom]]) { //"other" way as defined due to algorithmic reasons; apply method for other direction provided

  val maxStratum = map.keySet.reduce(math.max)

  private val stratumNr: Map[ExtendedAtom,Int] = ReverseFromSet(map)

  def apply(i:Int): Set[ExtendedAtom] = map(i)
  def apply(x:ExtendedAtom): Int =  stratumNr(x)

}

object Stratification {

  def apply(P: StdProgram): Option[Stratification] = {
    val depGraph = DepGraph(P)
    val condensation: QuotientGraph[ExtendedAtom] = Condensation(depGraph)
    // if any of these components contains an edge with dependency > (greater),
    // no stratification exists
    if (hasCycleWithGrt(depGraph, condensation)) {
      return None
    }
    //inherit arcs from original graph to blocks of condensation
    val condensationDepGraph: DepGraph[Block[ExtendedAtom]] = createCondensationDepGraph(depGraph,condensation)
    //if multiple nodes exist without incoming edges, introduce a single root node
    // (in order to have a single entry point for recursive top down numbering)
    val rootedDAG = ensureRooted(condensationDepGraph)
    val blockNumbering: Map[Block[ExtendedAtom], Int] = TopDownNumbering(rootedDAG)
    val idxToAtoms: Map[Int, Set[ExtendedAtom]] = createStratumMapping(blockNumbering)
    Option(Stratification(idxToAtoms))
  }

  /**
   * lift dependency edges from single edges to its blocks, where the strictest one wins,
   * i.e., block A > B if there is an edge a (in A) to b (in B) with label grt, else A >= B.
   * (there is no A=B since equality edges are merged upfront.)
   */
  def createCondensationDepGraph(depGraph: DepGraph[ExtendedAtom], condensation: QuotientGraph[ExtendedAtom]): DepGraph[Block[ExtendedAtom]] = {
    var depEdges = Set[DepEdge[Block[ExtendedAtom]]]()
    for ((fromBlock,setOfToBlocks) <- condensation.adjList) {
      for (toBlock <- setOfToBlocks) {
        val dep = findMostStrictDependency(depGraph,fromBlock,toBlock)
        depEdges += DepEdge[Block[ExtendedAtom]](fromBlock,toBlock,dep)
      }
    }
    DepGraph(condensation.nodes,depEdges)
  }

  def ensureRooted(g: DepGraph[Block[ExtendedAtom]]): DepGraph[Block[ExtendedAtom]] = {
    val startNodes: Set[Block[ExtendedAtom]] = g.startNodes()
    if (startNodes.size == 1) {
      return g
    }
    val root = new Block[ExtendedAtom]()
    val edges = startNodes.map{ e => DepEdge(root,e,geq) }
    g ++ edges
  }

  def findMostStrictDependency(depGraph: DepGraph[ExtendedAtom], fromBlock: Block[ExtendedAtom], toBlock: Block[ExtendedAtom]): Dependency = {
    for (fromNode <- fromBlock) {
      for (toNode <- toBlock) {
        if (depGraph.hasEdge(fromNode,toNode)) {
          if (depGraph.label(fromNode,toNode) == `grt`) {
            return `grt`
          }
        }
      }
    }
    return `geq` //`eq` not possible, these are already within a block
  }

  def hasCycleWithGrt(depGraph: DepGraph[ExtendedAtom], condensation: QuotientGraph[ExtendedAtom]): Boolean = {
    for (scc <- condensation.nodes) {
      val dg = depGraph.subgraph(scc)
      if (dg.edges.exists{ e => dg.label(e._1,e._2) == grt }) {
        return true
      }
    }
    false
  }

  def createStratumMapping(subgraphNr:Map[Block[ExtendedAtom],Int]): Map[Int, Set[ExtendedAtom]] = {
    var m = Map[Int,Set[ExtendedAtom]]()
    for ((nodes,nr) <- subgraphNr) {
      if (!nodes.isEmpty) { // may occur only for artificial root node (used for recursive top down numbering)
        if (m.contains(nr)) {
          val set = m(nr) ++ nodes
          m = m.updated(nr, set)
        } else {
          m = m.updated(nr, nodes)
        }
      }
    }
    m
  }
  
  def isStratification(strat: Map[ExtendedAtom,Int], P: StdProgram): Boolean = {    
    val G = DepGraph(P)
    for ((from,to) <- G.edges) {
      G.label(from,to) match {
        case `geq` => if (strat(from) < strat(to)) return false
        case `grt` => if (strat(from) <= strat(to)) return false
        case `eql` => if (strat(from) != strat(to)) return false
      }
    }
    return true
  }
}