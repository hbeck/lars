package lars.strat

import lars.core.semantics.formulas._
import lars.core.semantics.programs.extatoms._
import lars.core.semantics.programs.standard.{StdProgram, StdRule}
import lars.util.graph.{AdjList, Graph}

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Stream Dependency Graph
 * Created by hb on 7/6/15.
 */
case class DepGraph(V:Set[ExtendedAtom], depEdges:Set[DepEdge]) extends Graph[ExtendedAtom] with AdjList[ExtendedAtom] { //nodes added explicitly for unconnected ones

  val adjList: Map[ExtendedAtom,Set[ExtendedAtom]] = {
    val mMap = new collection.mutable.HashMap[ExtendedAtom,Set[ExtendedAtom]]()
    for (n <- V) {
      mMap += n -> Set[ExtendedAtom]()
    }
    for (edge@DepEdge(from, to, dep) <- depEdges) {
      mMap.update(from, mMap(from) + to)
    }
    mMap.toMap
  }

  def outgoing(n: ExtendedAtom): Set[ExtendedAtom] = adjList.getOrElse(n,Set())

  //subgraph induced by given nodes
  def subGraph(V: Set[ExtendedAtom]): DepGraph = {
    val s = collection.mutable.Set[DepEdge]()
    for (edge <- depEdges) {
      if (V.contains(edge.from) && V.contains(edge.to)) {
        s += edge
      }
    }
    DepGraph(V,s.toSet)
  }

  //remaining graph when removing the given nodes
  def -- (nodes: Set[ExtendedAtom]) : DepGraph = {
    val newNodes = this.nodes -- nodes
    subGraph(newNodes)
  }

  override def equals(other:Any) : Boolean = {
    other match {
      case g: DepGraph => this == g
      case _ => false
    }
  }

  def ==(other: DepGraph): Boolean = {
    this.nodes == other.nodes && this.depEdges == other.depEdges
  }
}

object DepGraph {

  def apply(P: StdProgram): DepGraph = {
    val nodes = ExtendedAtoms(P,true)
    val hba = headBodyArcs(P.rules,Set[DepEdge]())
    val na = nestingArcs(P)
    DepGraph(nodes, hba ++ na)
  }

  @tailrec
  private def headBodyArcs(rules: Set[StdRule], result: Set[DepEdge]) : Set[DepEdge] = {
    if (rules.isEmpty) {
      return result
    }
    val rule = rules.head

    var curr = mutable.HashSet[DepEdge]()
    for (beta <- rule.B) {
      curr += DepEdge(rule.h,beta,geq)
    }
    headBodyArcs(rules.tail, result ++ curr.toSet)
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