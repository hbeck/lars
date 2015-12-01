package lars.tms.cons

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.extatoms._
import lars.core.semantics.programs.standard.StdProgram

/**
 * Created by hb on 7/14/15.
 */
object ConsW {

  def apply(P: StdProgram, x: ExtendedAtom): Set[ExtendedAtom] = x match {
      case a:Atom => P.rules.flatMap(_.B).filter({
        case wa:WDiam => wa.a == a
        case wa:WBox => wa.a == a
        case wa:WAt => wa.a == a
        case _ => false})
      case aa:AtAtom => P.rules.flatMap(_.B).filter({
        case wa:WAt => /*(aa.t == wa.t) && */(aa.a == wa.a)
        case _ => false})
      case w:WindowAtom => val a = w.atom; P.rules.flatMap(_.B).filter({
        case wa:WDiam => wa.a == a
        case wa:WBox => wa.a == a
        case wa:WAt => wa.a == a
        case _ => false})
      case a => println("a: "+a);Set()
    }
}
