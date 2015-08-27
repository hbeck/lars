package lars.strat

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram
import lars.graph.DiGraph
import lars.graph.alg.BottomUpNumbering
import lars.graph.quotient.{Block, Condensation, QuotientGraph}

/**
 * Created by hb on 7/10/15.
 *
 */
object Stratify {

  /**
   * @param f takes a dependency graph and a condensation, and creates a new quotient graph
   * @return a Stream Stratification for program P, if it has one exists
   */
  def apply(P: StdProgram, f: DepQuotientGraph[ExtendedAtom] => DepQuotGraph[ExtendedAtom]): Option[Stratification] = {
    val depGraph = DepGraph(P)
    val condensation: QuotientGraph[ExtendedAtom] = Condensation(depGraph)
    // if any of these components contains an edge with dependency > (greater),
    // no stratification exists
    if (hasCycleWithGrt(depGraph, condensation)) {
      return None
    }
    Option(apply(depGraph,condensation,f))
  }

  /**
   * @param depGraph dependency graph
   * @param condensation quotient graph whose nodes are strongly connected components
   * @param f takes a dependency graph and a condensation, and creates a new quotient graph
   * @return a Stream Stratification
   */
  def apply(depGraph: DepGraph[ExtendedAtom], condensation: QuotientGraph[ExtendedAtom], f: DepQuotientGraph[Block[ExtendedAtom]] => DepQuotientGraph[ExtendedAtom]) : Stratification = {
    val condensationDepGraph: DepGraph[Block[ExtendedAtom]] = createCondensationDepGraph(depGraph,condensation)
    val stratumGraph: DepQuotient[ExtendedAtom] = f(condensationDepGraph)
    val subgraphNr: Map[Block[ExtendedAtom], Int] = BottomUpNumbering(stratumGraph)
    val nrToAtoms: Map[Int, Set[ExtendedAtom]] = createStratumMapping(subgraphNr)
    Stratification(nrToAtoms)
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
    return `geq`
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
  
  def createStratumMapping(subgraphNr:Map[Set[ExtendedAtom],Int]): Map[Int, Set[ExtendedAtom]] = {
    var m = Map[Int,Set[ExtendedAtom]]()
    for ((nodes,nr) <- subgraphNr) {
      if (m.contains(nr)) {
        val set = m(nr) ++ nodes
        m = m.updated(nr,set)
      } else {
        m = m.updated(nr,nodes)
      }
    }
    m
  }

}
