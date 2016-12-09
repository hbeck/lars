package lars.tms.cons

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.extatoms._
import lars.core.semantics.programs.standard.StdProgram

/**
 * Created by hb on 7/14/15.
 */
object ConsW {

  def apply(P: StdProgram, x: ExtendedAtom): Set[ExtendedAtom] =
    P.rules.flatMap(_.B) ++ P.rules.flatMap(_.head.atoms()) filter {
      case a: WindowAtom => a.nested.contains(x.atom)
      case _ => false
    }
}
