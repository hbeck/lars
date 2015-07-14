package lars.core.semantics.programs.general

import lars.core.semantics.formulas._

import scala.annotation.tailrec

/**
 * Created by hb on 7/13/15.
 */
object ExtendedAtoms {

  /**      
   * @return returns all maximal extended atoms appearing in the given formula, plus its nested atoms if nested==true
   */
  def apply(fm: Formula, nested: Boolean) : Set[ExtendedAtom] = {
    fm match {
      case a: Atom => Set(a)
      case Not(fm1) => apply(fm1, nested)
      case And(fm1, fm2) => apply(fm1, nested) union apply(fm2, nested)
      case Or(fm1, fm2) => apply(fm1, nested) union apply(fm2, nested)
      case Implies(fm1, fm2) => apply(fm1, nested) union apply(fm2, nested)
      case Diam(fm1) => apply(fm1, nested)
      case Box(fm1) => apply(fm1, nested)
      case At(u, fm1) =>
        if (fm1.isInstanceOf[Atom]) {
          val a = fm1.asInstanceOf[Atom]
          if (nested) {
            Set(a, AtAtom(u, a))
          } else {
            Set(AtAtom(u, a))
          }
        } else {
          apply(fm1, nested)
        }
      case W(wfn,x,fm,ch) => {
        val wfx = wfn.fix(x)
        val wop = new WindowOperatorFixedParams(wfx,ch)
        apply(Wop(wop,fm),nested)
      }
      case Wop(wop, fm1) => {
        fm1 match {
          case DiamAtom(a) =>
            if (nested) {
              Set(WDiamAtom(wop, DiamAtom(a)), a)
            } else {
              Set(WDiamAtom(wop, DiamAtom(a)))
            }
          case BoxAtom(a) =>
            if (nested) {
              Set(WBoxAtom(wop, BoxAtom(a)), a)
            } else {
              Set(WBoxAtom(wop, BoxAtom(a)))
            }
          case AtAtom(u, a) =>
            if (nested) {
              Set(WAtAtom(wop, AtAtom(u, a)), AtAtom(u, a), a)
            } else {
              Set(WAtAtom(wop, AtAtom(u, a)))
            }
          //
          case Diam(fm1) =>
            if (fm1.isInstanceOf[Atom]) {
              val a = fm1.asInstanceOf[Atom]
              apply(WDiamAtom(wop, DiamAtom(a)), nested)
            } else {
              apply(fm1, nested)
            }
          case Box(fm1) =>
            if (fm1.isInstanceOf[Atom]) {
              val a = fm1.asInstanceOf[Atom]
              apply(WBoxAtom(wop, BoxAtom(a)), nested)
            } else {
              apply(fm1, nested)
            }
          case At(u, fm1) =>
            if (fm1.isInstanceOf[Atom]) {
              val a = fm1.asInstanceOf[Atom]
              apply(WAtAtom(wop, AtAtom(u, a)), nested)
            } else {
              apply(fm1, nested)
            }
          //
          case x => apply(fm1, nested)
        }
      }
      //for extended atoms
      case DiamAtom(a) => Set(a)
      case BoxAtom(a) => Set(a)
      //extended atoms
      case AtAtom(u, a) =>
        if (nested) {
          Set(AtAtom(u, a), a)
        } else {
          Set(AtAtom(u, a))
        }
      case WDiamAtom(wop, da) =>
        if (nested) {
          Set(WDiamAtom(wop, da)) ++ apply(da, nested)
        } else {
          Set(WDiamAtom(wop, da))
        }
      case WBoxAtom(wop, ba) =>
        if (nested) {
          Set(WBoxAtom(wop, ba)) ++ apply(ba, nested)
        } else {
          Set(WBoxAtom(wop, ba))
        }
      case WAtAtom(wop, aa) =>
        if (nested) {
          Set(WAtAtom(wop, aa)) ++ apply(aa, nested)
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
