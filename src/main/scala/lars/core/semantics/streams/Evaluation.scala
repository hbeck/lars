package lars.core.semantics.streams

import lars.core.Util.merge
import lars.core.semantics.formulas.Atom

/**
 * Created by hb on 5/26/15.
 */
case class Evaluation(mapping:Map[Int,Set[Atom]]) extends (Int => Set[Atom]) {

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
    if (this.mapping.size != other.mapping.size) return false
    for (k <- mapping.keys) {
      val m0 = this.mapping.apply(k)
      val m1 = other.mapping.apply(k)
      if (!m0.equals(m1)) return false
    }
    true
  }
  def != (other: Evaluation) : Boolean = { !(this == other) }

  def ++ (other:Evaluation) = Evaluation(merge(mapping,other.mapping))

  override def equals(that:Any) = {
    that match {
      case other: Evaluation => this == other
      case _ => false
    }
  }

  def size : Int = Math.max(1,mapping.values.flatten.size)

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

