package lars.strat

import lars.core.semantics.formulas._
import lars.core.semantics.programs.{Program, Rule}

import scala.annotation.tailrec

/**
 * Created by hb on 7/6/15.
 */
object StratUtil {

  //TODO ugly...
  //
  //currently the body of a rule is formalized as conjunction
  //thus, sub only determines whether recursion is continued on the first occurrence of
  //an extended atom. above (in particular for And) we always recurse
  def extendedAtoms(fm: Formula, sub: Boolean): Set[ExtendedAtom] = {
    fm match {
      case a: Atom => Set(a)
      case Not(fm1) => extendedAtoms(fm1, sub)
      case And(fm1, fm2) => extendedAtoms(fm1, sub) union extendedAtoms(fm2, sub)
      case Or(fm1, fm2) => extendedAtoms(fm1, sub) union extendedAtoms(fm2, sub)
      case Implies(fm1, fm2) => extendedAtoms(fm1, sub) union extendedAtoms(fm2, sub)
      case Diam(fm1) => extendedAtoms(fm1, sub)
      case Box(fm1) => extendedAtoms(fm1, sub)
      case At(u, fm1) =>
        if (fm1.isInstanceOf[Atom]) {
          val a = fm1.asInstanceOf[Atom]
          if (sub) {
            Set(a, AtAtom(u, a))
          } else {
            Set(AtAtom(u, a))
          }
        } else {
          extendedAtoms(fm1, sub)
        }
      case W(wfn,x,fm,ch) => {
        val wfx = wfn.fix(x)
        val wop = new WindowOperatorFixedParams(wfx,ch)
        extendedAtoms(Wop(wop,fm),sub)
      }
      case Wop(wop, fm1) => {
        fm1 match {
          case DiamAtom(a) =>
            if (sub) {
              Set(WDiamAtom(wop, DiamAtom(a)), a)
            } else {
              Set(WDiamAtom(wop, DiamAtom(a)))
            }
          case BoxAtom(a) =>
            if (sub) {
              Set(WBoxAtom(wop, BoxAtom(a)), a)
            } else {
              Set(WBoxAtom(wop, BoxAtom(a)))
            }
          case AtAtom(u, a) =>
            if (sub) {
              Set(WAtAtom(wop, AtAtom(u, a)), AtAtom(u, a), a)
            } else {
              Set(WAtAtom(wop, AtAtom(u, a)))
            }
          //
          case Diam(fm1) =>
            if (fm1.isInstanceOf[Atom]) {
              val a = fm1.asInstanceOf[Atom]
              extendedAtoms(WDiamAtom(wop, DiamAtom(a)), sub)
            } else {
              extendedAtoms(fm1, sub)
            }
          case Box(fm1) =>
            if (fm1.isInstanceOf[Atom]) {
              val a = fm1.asInstanceOf[Atom]
              extendedAtoms(WBoxAtom(wop, BoxAtom(a)), sub)
            } else {
              extendedAtoms(fm1, sub)
            }
          case At(u, fm1) =>
            if (fm1.isInstanceOf[Atom]) {
              val a = fm1.asInstanceOf[Atom]
              extendedAtoms(WAtAtom(wop, AtAtom(u, a)), sub)
            } else {
              extendedAtoms(fm1, sub)
            }
          //
          case x => extendedAtoms(fm1, sub)
        }
      }
      //for extended atoms
      case DiamAtom(a) => Set(a)
      case BoxAtom(a) => Set(a)
      //extended atoms
      case AtAtom(u, a) =>
        if (sub) {
          Set(AtAtom(u, a), a)
        } else {
          Set(AtAtom(u, a))
        }
      case WDiamAtom(wop, da) =>
        if (sub) {
          Set(WDiamAtom(wop, da)) ++ extendedAtoms(da, sub)
        } else {
          Set(WDiamAtom(wop, da))
        }
      case WBoxAtom(wop, ba) =>
        if (sub) {
          Set(WBoxAtom(wop, ba)) ++ extendedAtoms(ba, sub)
        } else {
          Set(WBoxAtom(wop, ba))
        }
      case WAtAtom(wop, aa) =>
        if (sub) {
          Set(WAtAtom(wop, aa)) ++ extendedAtoms(aa, sub)
        } else {
          Set(WAtAtom(wop, aa))
        }
    }
  }

  @tailrec
  private def extendedAtoms(rules: Set[Rule], result: Set[ExtendedAtom], sub: Boolean): Set[ExtendedAtom] = {
    if (rules isEmpty) {
      return result
    }
    val rule = rules.head
    val curr = extendedAtoms(rule.head, sub) ++ extendedAtoms(rule.body, sub)
    extendedAtoms(rules.tail, result ++ curr, sub)
  }

  def extendedAtoms(rules: Set[Rule], sub: Boolean): Set[ExtendedAtom] = {
    extendedAtoms(rules,Set[ExtendedAtom](),sub)
  }

  //\mathcal{A}^+ if sub == false, else \mathcal{A}^+_{sub}
  def extendedAtoms(P: Program, sub: Boolean): Set[ExtendedAtom] = {
    extendedAtoms(P.rules, sub)
  }

}
