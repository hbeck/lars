package lars.core.semantics.structure

import lars.core.semantics.formulas.Atom
import lars.core.semantics.programs.{Program, Reduct, Rule}
import lars.core.semantics.streams.S

/**
 * Created by hb on 7/15/15.
 */
object IsAnswerStream {

  def apply[R <: Rule](I: S, P: Program[R], D:S, t:Int, B:Set[Atom]=Set()): Boolean = {
    val M = I.toStructure(B)
    val R = Reduct(P,M,t)
    M.isMinimalModel(R,t,D)
  }

}
