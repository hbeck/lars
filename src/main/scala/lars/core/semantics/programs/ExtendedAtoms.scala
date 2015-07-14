package lars.core.semantics.programs

import lars.core.semantics.formulas._
import lars.core.semantics.programs.general.{GeneralProgram, GeneralRule}

import scala.annotation.tailrec

/**
 * Created by hb on 7/13/15.
 */
@deprecated
object ExtendedAtoms {

  def apply(fm: Formula, sub: Boolean) : Set[ExtendedAtom] = {
    fm match {
      case a: Atom => Set(a)
      case Not(fm1) => apply(fm1, sub)
      case And(fm1, fm2) => apply(fm1, sub) union apply(fm2, sub)
      case Or(fm1, fm2) => apply(fm1, sub) union apply(fm2, sub)
      case Implies(fm1, fm2) => apply(fm1, sub) union apply(fm2, sub)
      case Diam(fm1) => apply(fm1, sub)
      case Box(fm1) => apply(fm1, sub)
      case At(u, fm1) =>
        if (fm1.isInstanceOf[Atom]) {
          val a = fm1.asInstanceOf[Atom]
          if (sub) {
            Set(a, AtAtom(u, a))
          } else {
            Set(AtAtom(u, a))
          }
        } else {
          apply(fm1, sub)
        }
      case W(wfn,x,fm,ch) => {
        val wfx = wfn.fix(x)
        val wop = new WindowOperatorFixedParams(wfx,ch)
        apply(Wop(wop,fm),sub)
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
              apply(WDiamAtom(wop, DiamAtom(a)), sub)
            } else {
              apply(fm1, sub)
            }
          case Box(fm1) =>
            if (fm1.isInstanceOf[Atom]) {
              val a = fm1.asInstanceOf[Atom]
              apply(WBoxAtom(wop, BoxAtom(a)), sub)
            } else {
              apply(fm1, sub)
            }
          case At(u, fm1) =>
            if (fm1.isInstanceOf[Atom]) {
              val a = fm1.asInstanceOf[Atom]
              apply(WAtAtom(wop, AtAtom(u, a)), sub)
            } else {
              apply(fm1, sub)
            }
          //
          case x => apply(fm1, sub)
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
          Set(WDiamAtom(wop, da)) ++ apply(da, sub)
        } else {
          Set(WDiamAtom(wop, da))
        }
      case WBoxAtom(wop, ba) =>
        if (sub) {
          Set(WBoxAtom(wop, ba)) ++ apply(ba, sub)
        } else {
          Set(WBoxAtom(wop, ba))
        }
      case WAtAtom(wop, aa) =>
        if (sub) {
          Set(WAtAtom(wop, aa)) ++ apply(aa, sub)
        } else {
          Set(WAtAtom(wop, aa))
        }
    }
  }

  @tailrec
  private def apply(rules: Set[GeneralRule], result: Set[ExtendedAtom], sub: Boolean): Set[ExtendedAtom] = {
    if (rules.isEmpty) {
      return result
    }
    val rule = rules.head
    val curr = apply(rule.head, sub) ++ apply(rule.body, sub)
    apply(rules.tail, result ++ curr, sub)
  }

  def apply(rules: Set[GeneralRule], sub: Boolean): Set[ExtendedAtom] = {
    apply(rules,Set[ExtendedAtom](),sub)
  }

  //\mathcal{A}^+ if sub == false, else \mathcal{A}^+_{sub}
  def apply(P: GeneralProgram, sub: Boolean): Set[ExtendedAtom] = {
    apply(P.rules, sub)
  }

}
