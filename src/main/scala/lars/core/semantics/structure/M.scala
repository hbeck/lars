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

  //wrt data stream D
  def isMinimalModel(P: Program, t: Int, D: S): Boolean = {
    //TODO
    false
  }

}
