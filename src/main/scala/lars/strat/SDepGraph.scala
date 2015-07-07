package lars.strat

import lars.core.semantics.formulas._
import lars.core.semantics.programs.{Rule, Program}

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Stream Depenedency Graph
 * Created by hb on 7/6/15.
 */
case class SDepGraph(edges:Set[SDepEdge])

object SDepGraph {

  def from(P: Program): SDepGraph = {
    val hba = headBodyArcs(P.rules,Set[SDepEdge]())
    val na = nestingArcs(P)
    SDepGraph(hba ++ na)
  }

  @tailrec
  private def headBodyArcs(rules: Set[Rule], result: Set[SDepEdge]) : Set[SDepEdge] = {
    if (rules isEmpty) {
      return result
    }
    val rule = rules.head
    val alpha = rule.head.asInstanceOf[ExtendedAtom] //TODO not automatic at the moment due to 'dual' ontology
    val betas: Set[ExtendedAtom] = StratUtil.extendedAtoms(rule.body, false)

    var curr = mutable.HashSet[SDepEdge]()
    for (beta <- betas) {
      curr += SDepEdge(alpha,beta,geq)
    }
    headBodyArcs(rules.tail, result ++ curr.toSet)
  }

  private def nestingArcs(P: Program): Set[SDepEdge] = {
    val eats = StratUtil.extendedAtoms(P,true)
    var mset = mutable.HashSet[SDepEdge]()
    for (ea <- eats) {
      ea match { //consider only 'relevant' once - see TODO marks concerning dual ontology
        case AtAtom(u, a) => {
          mset += SDepEdge(AtAtom(u,a),a,eql)
          mset += SDepEdge(a,AtAtom(u,a),eql)
        }
        //TODO unify following three cases
        case WDiamAtom(wop, da) => {
          mset += SDepEdge(WDiamAtom(wop,da),da.a,grt)
        }
        case WBoxAtom(wop, ba) => {
          mset += SDepEdge(WBoxAtom(wop,ba),ba.a,grt)
        }
        case WAtAtom(wop, aa) => {
          mset += SDepEdge(WAtAtom(wop,aa),aa.a,grt)
        }
        case x => if (!x.isInstanceOf[Atom]) { assert(false) } //atoms are only to-Nodes
      }
    }
    mset.toSet
  }
}