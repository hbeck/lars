package lars.core.semantics.programs.general.inspect

import lars.core.semantics.formulas.{Unary, Atom}
import lars.core.semantics.programs.{Program, Rule}

/**
 * Created by hb on 8/5/15.
 */
object TemporallyQuantifiedInHead {

  def apply[R <: Rule](P: Program[R]): Set[Atom] = {
    P.rules.map(_.head).
      flatMap(Subformulas(_)).
      filter(IsTemporallyQuantified(_)).
      map(_.asInstanceOf[Unary]).
      filter(_.fm.isInstanceOf[Atom]).
      map(_.fm.asInstanceOf[Atom])
  }

}
