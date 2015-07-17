package lars.strat

import lars.core.semantics.formulas._
import lars.core.semantics.programs.extatoms._
import lars.core.semantics.programs.standard.{StdProgram, StdRule}
import lars.graph.{DiGraph, LabeledDiGraph}

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
 * Stream Dependency Graph
 * Created by hb on 7/6/15.
 */
//TODO multiple labels per edge possible!
case class DepGraph(override val adjList: Map[ExtendedAtom,Set[ExtendedAtom]],
                    override val label: (ExtendedAtom,ExtendedAtom) => Dependency)

  extends LabeledDiGraph[ExtendedAtom,Dependency](adjList,label) {

  override def outgoing(n: ExtendedAtom): Set[ExtendedAtom] = adjList.getOrElse(n,Set())

  override def subgraph(vertices: Set[ExtendedAtom]): DepGraph = {
    val digraph: DiGraph[ExtendedAtom] = super.subgraph(vertices)
    DepGraph(digraph.adjList, label) //reduce label "keys"?
  }

  //remaining graph when removing the given nodes
  def -- (nodes: Set[ExtendedAtom]) : DepGraph = {
    val newNodes = this.nodes -- nodes
    subgraph(newNodes)
  }

  override def equals(other:Any) : Boolean = {
    other match {
      case g: DepGraph => this == g
      case _ => false
    }
  }

}

object DepGraph {

  def apply(P: StdProgram): DepGraph = {
    val nodes = ExtendedAtoms(P,true)
    val hba = headBodyArcs(P)
    val na = nestingArcs(P)
    val depEdges: Set[DepEdge] = hba ++ na
    apply(nodes,depEdges)
  }

  def apply(nodes: Set[ExtendedAtom], depEdges: Set[DepEdge]): DepGraph = {
    val adjList = createAdjList(nodes,depEdges)
    //abstract away dependencies into label function (as map):
    var m = Map[(ExtendedAtom,ExtendedAtom),Dependency]()
    for (e@DepEdge(from,to,dep) <- depEdges) {
      m = m + ((from,to) -> dep)
    }
    val label = { (x:ExtendedAtom,y:ExtendedAtom) => m((x,y)) }
    DepGraph(adjList,label)
  }

  def createAdjList(vertices: Set[ExtendedAtom], edges:Set[DepEdge]): Map[ExtendedAtom,Set[ExtendedAtom]] = {
    var m = HashMap[ExtendedAtom,Set[ExtendedAtom]]()
    for (n <- vertices) {
      m = m + (n -> Set[ExtendedAtom]())
    }
    for (e@DepEdge(from,to,dep) <- edges) {
      m = m.updated(from, m(from)+to)
    }
    m
  }
  
  private def headBodyArcs(P: StdProgram): Set[DepEdge] = {
    headBodyArcsImpl(P.rules, Set[DepEdge]())
  }

  @tailrec
  private def headBodyArcsImpl(rules: Set[StdRule], edges: Set[DepEdge]) : Set[DepEdge] = {
    if (rules.isEmpty) {
      return edges
    }
    val rule = rules.head

    val curr = mutable.HashSet[DepEdge]()
    for (beta <- rule.B) {
      curr += DepEdge(rule.h,beta,geq)
    }
    headBodyArcsImpl(rules.tail, edges ++ curr.toSet)
  }

  private def nestingArcs(P: StdProgram): Set[DepEdge] = {
    val xs = ExtendedAtoms(P,true)
    var mset = mutable.HashSet[DepEdge]()
    for (x <- xs) {
      x match { //consider only 'relevant' ones
        case y:AtAtom => {
          mset += DepEdge(AtAtom(y.t,y.a),y.a,eql)
          mset += DepEdge(y.a,AtAtom(y.t,y.a),eql)
        }
        case y:WindowAtom => {
          mset += DepEdge(y,y.atom,grt)
        }
        case x => if (!x.isInstanceOf[Atom]) { assert(false) } //atoms are only to-Nodes
      }
    }
    mset.toSet
  }
}