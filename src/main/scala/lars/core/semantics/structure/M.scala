package lars.core.semantics.structure

import lars.core.semantics.formulas.Atom
import lars.core.semantics.programs.{Rule, Program}
import lars.core.semantics.streams.{Evaluation, S, Timeline}

/**
 * Created by hb on 1/2/15.
 */
//M - Structure
case class M(T: Timeline, v: Evaluation, B: Set[Atom]) {

  def /(stream: S) = MS(this, stream)
  def /(t: Int) = Mt(this,t)

  def isModel[R <: Rule](P: Program[R], t: Int): Boolean = {
    this/t |= P
  }

  override def toString = {
    "<"+T+","+v+","+B+">"
  }

  //wrt data stream D
  def isMinimalModel[R <: Rule](P: Program[R], t: Int, D: S): Boolean = {

    if (!isModel(P,t)) return false

    //naive implementation, for the moment: try if any subset
    //of the intentional part + D is a model
    var intentionalPart = S(T,v) -- D

    //iterate over substreams of intentional part, add it to D, and see if it is a model
    //in case one is found, this is not a minimal model
    for (addS <- intentionalPart.properSubstreams()) {
      val augD = D ++ addS
      val candidateM = augD.toStructure(B)
      if (candidateM / t |= P) {
        //println("smaller model: " + candidateM)
        return false
      }
    }
    true
  }

}
