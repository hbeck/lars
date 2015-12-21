package lars.strat

import lars.core.semantics.formulas._
import lars.core.semantics.programs.extatoms.{AtAtom, _}
import lars.core.semantics.programs.standard.{StdProgram, StdRule}
import lars.graph.{DiGraph, LabeledDiGraph}
import lars.util.map.Merge

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
 * Stream Dependency Graph
 * Created by hb on 7/6/15.
 */
class DepGraph[V](override val adjList: Map[V,Set[V]],
                  override val label: (V,V) => Dependency)
  extends LabeledDiGraph[V,Dependency](adjList,label) {

  override def outgoing(n: V): Set[V] = adjList.getOrElse(n,Set())

  override def subgraph(vertices: Set[V]): DepGraph[V] = {
    val digraph: DiGraph[V] = super.subgraph(vertices)
    new DepGraph[V](digraph.adjList, label) //reduce label "keys"?
  }

  //TODO other symbol than --
  //remaining graph when removing the given nodes
  def -- (nodes: Set[V]) : DepGraph[V] = {
    val newNodes = this.nodes -- nodes
    subgraph(newNodes)
  }

  //TODO other symbol than ++
  def ++ (depEdges: Set[DepEdge[V]]): DepGraph[V] = {
    val newNodes = this.nodes ++ depEdges.flatMap{ e => Set[V](e.from,e.to) }
    val diffAdjList: Map[V, Set[V]] = DepGraph.createAdjList(newNodes, depEdges)
    val newAdjList = Merge(this.adjList,diffAdjList)
    new DepGraph[V](newAdjList,DepGraph.mkLabel(depEdges ++ this.depEdges))
  }
  
  def depEdges(): Set[DepEdge[V]] = edges.map{ e:(V,V) => DepEdge(e._1,e._2,label(e._1,e._2)) }

  override def equals(other:Any) : Boolean = {
    other match {
      case g: DepGraph[V] => this == g
      case _ => false
    }
  }

}

object DepGraph {

  def apply(P: StdProgram): DepGraph[ExtendedAtom] = {
    val nodes: Set[ExtendedAtom] = ExtendedAtoms(P,true)
    val hba = headBodyArcs(P)
    val na = nestingArcs(P)
    val depEdges: Set[DepEdge[ExtendedAtom]] = hba ++ na
    apply(nodes,depEdges)
  }

  def apply[V](nodes: Set[V], depEdges: Set[DepEdge[V]]): DepGraph[V] = {
    val adjList = createAdjList(nodes,depEdges)
    //abstract away dependencies into label function (as map):
    new DepGraph[V](adjList,mkLabel(depEdges))
  }

  def mkLabel[V](depEdges: Set[DepEdge[V]]): ((V,V) => Dependency) = {
    var m = Map[(V,V),Dependency]()
    for (e@DepEdge(from,to,dep) <- depEdges) {
      m = m + ((from,to) -> dep)
    }
    val imm = m.toMap
    val labelFn = { (x:V,y:V) => imm((x,y)) }
    labelFn
  }

  def createAdjList[V](vertices: Set[V], edges:Set[DepEdge[V]]): Map[V,Set[V]] = {
    var m = HashMap[V,Set[V]]()
    for (n <- vertices) {
      m = m + (n -> Set[V]())
    }
    for (e@DepEdge(from,to,dep) <- edges) {
      m = m.updated(from, m(from)+to)
    }
    m
  }
  
  private def headBodyArcs(P: StdProgram): Set[DepEdge[ExtendedAtom]] = {
    headBodyArcsImpl(P.rules, Set[DepEdge[ExtendedAtom]]())
  }

  @tailrec
  private def headBodyArcsImpl(rules: Set[StdRule], edges: Set[DepEdge[ExtendedAtom]]) : Set[DepEdge[ExtendedAtom]] = {
    if (rules.isEmpty) {
      return edges
    }
    val rule = rules.head

    val curr = mutable.HashSet[DepEdge[ExtendedAtom]]()
    for (beta <- rule.B) {
      val dep = determineHeadBodyDependency(rule.h,beta)
      curr += DepEdge(rule.h,beta,dep)
    }
    headBodyArcsImpl(rules.tail, edges ++ curr.toSet)
  }

  /*
   * normally, the head-body dependency is grt (>=), but for a rule "@_t a <- a, ..."
   * we have to make sure that eql (=) is used.
  */
  def determineHeadBodyDependency(h: ExtendedAtom, b: ExtendedAtom) : Dependency = {
    h match {
      case x:AtAtom => {
        if (b.isInstanceOf[Atom] && b.asInstanceOf[Atom] == x.atom) {
          return eql
        } else {
          return geq
        }
      }
      case _ => geq
    }
  }

  private def nestingArcs(P: StdProgram): Set[DepEdge[ExtendedAtom]] = {
    val xs = ExtendedAtoms(P,true)
    var mset = mutable.HashSet[DepEdge[ExtendedAtom]]()
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