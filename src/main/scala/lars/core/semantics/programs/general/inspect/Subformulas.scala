package lars.core.semantics.programs.general.inspect

import lars.core.semantics.formulas.{Formula, _}

/**
 * Created by hb on 8/5/15.
 */
object Subformulas {

  def apply(fm: Formula): Set[Formula] = {
    Set(fm) ++ {
      fm match {
        case Verum => Set()
        case Falsum => Set()
        case a: Atom => Set(a)
        case u: Unary => Subformulas(u.fm)
        case b: Binary => Subformulas(b.fm1) ++ Subformulas(b.fm2)
      }
    }
  }

}
