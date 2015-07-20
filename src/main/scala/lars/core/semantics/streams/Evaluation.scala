package lars.core.semantics.streams

import lars.util.map.Merge
import lars.core.semantics.formulas.Atom

import scala.collection.immutable.HashMap

/**
 * Created by hb on 5/26/15.
 */
case class Evaluation(mapping:Map[Int,Set[Atom]]=new HashMap[Int,Set[Atom]]) extends (Int => Set[Atom]) {

  def apply(t:Int) = mapping.getOrElse(t,Set[Atom]())

  //restriction
  def | (T: Timeline) = Evaluation(mapping filterKeys { T contains _ })

  def <= (other: Evaluation) = {
    def subseteqAt(t:Int) = {
      apply(t) subsetOf (other (t)) //TODO
    }
    mapping.keys.forall { subseteqAt }
  }

  def == (other: Evaluation) : Boolean = {
    if (this.size != other.size) return false
    for (t <- this.mapping.keys) {
      val m0 = this.mapping(t)
      val m1 = other.mapping.getOrElse(t,Set())
      if (m0 != m1) return false
    }
    true
  }

  def != (other: Evaluation) : Boolean = { !(this == other) }

  def ++ (other:Evaluation) = Evaluation(Merge(mapping,other.mapping))

  override def equals(that:Any) = {
    that match {
      case other: Evaluation => this == other
      case _ => false
    }
  }

  def size = mapping.values.flatten.size

  //note: paper defines size one for empty stream
  def nonZeroSize: Int = Math.max(1,mapping.values.flatten.size)

  override def toString = {
    val sb = new StringBuilder()
    sb.append("{")
    for ((k, v) <- mapping) {
      sb.append(" " + k + " -> " + v)
    }
    sb.append(" }")
    sb.toString()
  }
}

object Evaluation {
  def fromTimestampedAtoms(tsAtoms: Set[(Int,Atom)]): Evaluation = {
    var m: HashMap[Int,Set[Atom]] = HashMap()
    for (tsAtom <- tsAtoms) {
      val k = tsAtom._1
      val atom = tsAtom._2
      if (m contains k) {
        val ats = (m apply k) + atom
        m += (k -> ats)
      } else {
        m += k -> Set[Atom](atom)
      }
    }
    Evaluation(m.toMap)
  }
}