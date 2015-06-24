package lars.core.semantics.streams

import lars.core.semantics.formulas.Atom
import lars.core.semantics.programs.Program
import lars.core.semantics.structure.M

import scala.collection.mutable.HashMap
import scala.collection.immutable.Map

/**
 * Created by hb on 5/26/15.
 */
case class S(T: Timeline, v: Evaluation) {

  def <= (other:S) = {
    (this.T <= other.T) && (this.v <= other.v)
  }

  def size = v.size

  def | (T1: Timeline) = S(T1,v|T1)

  def == (other: S) : Boolean = {
    this.T == other.T && this.v == other.v
  }

  def != (other: S) : Boolean = !(this == other)

  //TODO limitation of fp approach? mutable: insert into set a single atom,
  //vs copying (at least iterating) the whole thing
  def + (tsAtom: (Int,Atom)) : S = {
    val t = tsAtom._1
    val atom = tsAtom._2
    var ats : Set[Atom] = null
    if (v.mapping contains t) {
      ats = (v.mapping apply t) + atom
    } else {
      ats = Set[Atom](atom)
    }
    var m:HashMap[Int,Set[Atom]] = HashMap()
    for (k <- v.mapping.keys) {
      if (k == t) {
        m += (t -> ats)
      } else {
        m += (k -> v.mapping.apply(k))
      }
    }
    S(T,Evaluation(m.toMap))
  }

  def ++ (other: S) = S(T ++ other.T, v ++ other.v)

  //'setminus'
  def -- (other: S) = {
    //better: i) map builder ii) work directly on Evaluation object
    var m = new HashMap[Int,Set[Atom]]()
    for (k <- v.mapping.keys) {
      val thisV:Set[Atom] = this.v.mapping.apply(k)
      val otherV:Set[Atom] = other.v.mapping.getOrElse(k, Set[Atom]().empty)
      val diffV:Set[Atom] = thisV -- otherV
      m += k -> diffV
    }
    val im:Map[Int,Set[Atom]] = m.toMap //immutable
    S(T,Evaluation(im))
  }

  def getTimestampedAtoms():Set[(Int,Atom)] = {
    var s:Set[(Int,Atom)] = Set()
    for (t <- v.mapping.keys) {
      for (atom <- v.mapping.apply(t)) {
        s += ((t,atom))
      }
    }
    s.toSet
  }

  override def equals(that:Any) : Boolean = {
    that match {
      case other: S => this == other
      case _ => false
    }
  }

  def toStructure() = M(T,v,Set[Atom]())
  def toStructure(B:Set[Atom]) = M(T,v,B)

  def isAnswerStream(P:Program, D:S, t:Int, B:Set[Atom]=Set()) : Boolean = {

    //TODO assert this is a superstream of D with fresh atoms
    //
    val M = this.toStructure(B)
    val R = P.reduct(M,t)
    M.isMinimalModel(R,t,D)
  }

}

object S {
  def fromTimestampedAtoms(T:Timeline, tsAtoms: Set[(Int,Atom)]): S = {
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
    S(T,Evaluation(m.toMap))
  }
}


