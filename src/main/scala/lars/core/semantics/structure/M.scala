package lars.core.semantics.structure

import lars.core.semantics.formulas.Atom
import lars.core.semantics.programs.Program
import lars.core.semantics.streams.{Evaluation, S, Timeline}

/**
 * Created by hb on 1/2/15.
 */
//M - Structure
case class M(T: Timeline, v: Evaluation, B: Set[Atom]) {

  def /(stream: S) = MS(this, stream)
  def /(t: Int) = Mt(this,t)

  def isModel(P: Program, t: Int): Boolean = {
    this/t |= P
  }

  override def toString = {
    "<"+T+","+v+","+B+">"
  }

  //wrt data stream D
  def isMinimalModel(P: Program, t: Int, D: S): Boolean = {

    if (!isModel(P,t)) return false

    //naive implementation, for the moment: try if any subset
    //of the intentional part + D is a model

    var intentionalPart = S(T,v) -- D

    //from all mappings k -> {v1, ..., vn} create a set
    //k -> v1, ..., k -> vn;
    //create a set of all these single-atom mappings (for all keys)
    var tsAtoms:Set[(Int,Atom)] = Set()
    for (k <- intentionalPart.v.mapping.keys) {
      val tsAtomsK = intentionalPart.getTimestampedAtoms()
      tsAtoms ++= tsAtomsK
    }
    val timestampedAtoms = tsAtoms.toSet

    //iterate over the power set, add it to D, and see if it is a model
    //in case one is found, this is not a minimal model
    for (subset <- timestampedAtoms.subsets()) {
      if (subset != timestampedAtoms) {
        val augD = D ++ S.fromTimestampedAtoms(T, subset)
        val candidateM = augD.toStructure(B)
        if (candidateM / t |= P) {
          println("smaller model: " + candidateM)
          return false
        }
      }
    }
    true
  }

}
