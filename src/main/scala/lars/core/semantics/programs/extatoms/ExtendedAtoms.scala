package lars.core.semantics.programs.extatoms

import lars.core.semantics.formulas._
import lars.core.semantics.programs.standard.StdProgram
import lars.core.semantics.programs.{Program, Rule}
import lars.core.semantics.formulas.Unary

import scala.annotation.tailrec

object ExtendedAtoms {

  def apply(P: StdProgram) : Set[ExtendedAtom] = {
    P.rules.flatMap( r => r.B + r.h)
  }

  /**
   * @return returns all maximal extended atoms appearing in the given formula, plus its nested atoms if nested==true
   */
  def apply(fm: Formula, nested: Boolean = true): Set[ExtendedAtom] = {
    fm match {
      case Verum => Set()
      case Falsum => Set()
      //case a: Atom => Set(a)
      case x: ExtendedAtom => {
        if (nested) {
          x.nested
        } else {
          Set(x)
        }
      }
      //Whether At, W, Wop are (different encodings) of ExtendedAtoms needs to be tested ...
      case At(u, fm1) =>
        if (fm1.isInstanceOf[Atom]) {
          apply(AtAtom(u, fm1.asInstanceOf[Atom]), nested)
        } else {
          apply(fm1, nested)
        }
      case W(wfn, x, fm, ch) => {
        //convert to match uniformly below
        val wfx = wfn.fix(x)
        val wop = new WindowOperatorFixedParams(wfx, ch)
        apply(WindowFormula(wop, fm), nested)
      }
      case WindowFormula(wop, fm1) => {
        fm1 match {
          case Diam(fm2) =>
            if (fm2.isInstanceOf[Atom]) {
              apply(WDiam(wop, fm2.asInstanceOf[Atom]), nested)
            } else {
              apply(fm2, nested)
            }
          case Box(fm2) =>
            if (fm2.isInstanceOf[Atom]) {
              apply(WBox(wop, fm2.asInstanceOf[Atom]), nested)
            } else {
              apply(fm2, nested)
            }
          case At(u, fm2) =>
            if (fm2.isInstanceOf[Atom]) {
              apply(WAt(wop, u, fm2.asInstanceOf[Atom]), nested)
            } else {
              apply(fm2, nested)
            }
          //
          case x => apply(fm1, nested)
        }
      }
      //... for all other formulas, only the nested expressions need to be tested (if nested==true)
      case x:Unary => apply(x.fm, nested)
      case x:Binary => apply(x.fm1, nested) union apply(x.fm2, nested)
    }
  }

  def apply[R <: Rule](rules: Set[R], nested: Boolean): Set[ExtendedAtom] = {
    @tailrec
    def fn[R <: Rule](rules: Set[R], result: Set[ExtendedAtom], nested: Boolean): Set[ExtendedAtom] = {
      if (rules.isEmpty) {
        return result
      }
      val rule = rules.head
      val curr = apply(rule.head, nested) ++ apply(rule.body, nested)
      fn(rules.tail, result ++ curr, nested)
    }
    fn(rules, Set[ExtendedAtom](), nested)
  }

  //\mathcal{A}^+ if sub == false, else \mathcal{A}^+_{sub}
  def apply[R <: Rule](P: Program[R], nested: Boolean): Set[ExtendedAtom] = {
    apply(P.rules, nested)
  }
}