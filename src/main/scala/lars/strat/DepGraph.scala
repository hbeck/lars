package lars.strat

import lars.core.semantics.formulas._
import lars.core.semantics.programs.extatoms._
import lars.core.semantics.programs.{Program, Rule}

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Stream Dependency Graph
 * Created by hb on 7/6/15.
 */
case class DepGraph(nodes:Set[ExtendedAtom], edges:Set[DepEdge]) { //nodes added explicitly for unconnected ones

  val adjList: Map[ExtendedAtom,Set[ExtendedAtom]] = {
    val mMap = new collection.mutable.HashMap[ExtendedAtom,Set[ExtendedAtom]]()
    for (n <- nodes) {
      mMap += n -> Set[ExtendedAtom]()
    }
    for (edge@DepEdge(from, to, dep) <- edges) {
      mMap.update(from, mMap(from) + to)
    }
    mMap.toMap
  }

  def neighbours(n: ExtendedAtom): Set[ExtendedAtom] = adjList.getOrElse(n,Set())

//  def nodes(): Set[ExtendedAtom] = {
//    @tailrec
//    def nodes(xs:Set[DepEdge], result: Set[ExtendedAtom]): Set[ExtendedAtom] = {
//      if (xs.isEmpty) {
//        return result
//      }
//      val e = xs.head
//      nodes(xs.tail, result ++ Set(e.from,e.to))
//    }
//    nodes(edges,Set[ExtendedAtom]())
//  }

  //subgraph induced by given nodes
  def subGraph(nodes: Set[ExtendedAtom]): DepGraph = {
    val s = collection.mutable.Set[DepEdge]()
    for (edge <- edges) {
      if (nodes.contains(edge.from) && nodes.contains(edge.to)) {
        s += edge
      }
    }
    DepGraph(nodes,s.toSet)
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
    this.nodes == other.nodes && this.edges == other.edges
  }
}

object DepGraph {

  def apply[R <: Rule, Pr <: Program[R]](P: Pr): DepGraph = {
    val nodes = ExtendedAtoms(P,true) //TODO true okay?
    val hba = headBodyArcs(P.rules,Set[DepEdge]())
    val na = nestingArcs(P)
    DepGraph(nodes, hba ++ na)
  }

  @tailrec
  private def headBodyArcs[R <: Rule](rules: Set[R], result: Set[DepEdge]) : Set[DepEdge] = {
    if (rules.isEmpty) {
      return result
    }
    val rule = rules.head
    val alpha = ExtendedAtoms(rule.head, false).head
    val betas: Set[ExtendedAtom] = ExtendedAtoms(rule.body, false)

    var curr = mutable.HashSet[DepEdge]()
    for (beta <- betas) {
      curr += DepEdge(alpha,beta,geq)
    }
    headBodyArcs(rules.tail, result ++ curr.toSet)
  }

  private def nestingArcs[R <: Rule, Pr <: Program[R]](P: Pr): Set[DepEdge] = {
    val xs = ExtendedAtoms(P,true)
    var mset = mutable.HashSet[DepEdge]()
    for (x <- xs) {
      x match { //consider only 'relevant' ones
        case y:AtAtom => {
          mset += DepEdge(AtAtom(y.t,y.a),y.a,eql)
          mset += DepEdge(y.a,AtAtom(y.t,y.a),eql)
        }
        //TODO unify following three cases
//        case y:WindowAtom => {
//          mset += DepEdge(y,y.atom,grt)
//        }
        case y:WDiam => {
          mset += DepEdge(WDiam(y.wop,y.a),y.a,grt)
        }
        case y:WBox => {
          mset += DepEdge(WBox(y.wop,y.a),y.a,grt)
        }
        case y:WAt => {
          mset += DepEdge(WAt(y.wop,y.t,y.a),y.a,grt)
        }
        case x => if (!x.isInstanceOf[Atom]) { assert(false) } //atoms are only to-Nodes
      }
    }
    mset.toSet
  }
}