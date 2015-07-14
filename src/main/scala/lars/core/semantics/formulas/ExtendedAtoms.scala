package lars.core.semantics.formulas

import lars.core.semantics.formulas.WindowOperators.{StreamChoice, ch2}
import lars.core.semantics.programs.{Program, Rule}
import lars.core.windowfn.WindowFunctionFixedParams

import scala.annotation.tailrec

/**
 * Created by hb on 7/10/15.
 */
case class WindowOperatorFixedParams(wfn: WindowFunctionFixedParams, ch:StreamChoice=ch2) {
  override def toString = "⊞^{"+wfn+"}"
}

case class AtAtom(override val t: Int, a: Atom) extends At(t,a) with ExtendedAtom {
  override def toString = "@{"+t+"}"+a
  override def atom = a
}
case class WAtAtom(w: WindowOperatorFixedParams, t:Int, a: Atom) extends Wop(w,At(t,a)) with ExtendedAtom {
  override def toString = w + "@{" + t+ "}"+a
  override def atom = a
}
case class WDiamAtom(w: WindowOperatorFixedParams, a: Atom) extends Wop(w,Diam(a)) with ExtendedAtom {
  override def toString = w + "◇" + a
  override def atom = a
}
case class WBoxAtom(w: WindowOperatorFixedParams, a: Atom) extends Wop(w,Box(a)) with ExtendedAtom {
  override def toString = w + "☐" + a
  override def atom = a
}

object AtAtom {
 def apply(x:At) = AtAtom(x.t,x.fm.asInstanceOf[Atom])
}

object WAtAtom {
  def apply(x:Wop) = {
    val at = x.fm.asInstanceOf[At]
    WAtAtom(x.wop,at.t,at.fm.asInstanceOf[Atom])
  }
}

object WDiamAtom {
  def apply(x:Wop) = {
    val diamAtom = x.fm.asInstanceOf[Diam]
    WDiamAtom(x.wop,diamAtom.fm.asInstanceOf[Atom])
  }
}

object WBoxAtom {
  def apply(x:Wop) = {
    val boxAtom = x.fm.asInstanceOf[Box]
    WBoxAtom(x.wop,boxAtom.fm.asInstanceOf[Atom])
  }
}

object ExtendedAtoms {

  /**
   * @return returns all maximal extended atoms appearing in the given formula, plus its nested atoms if nested==true
   */
  def apply(fm: Formula, nested: Boolean): Set[ExtendedAtom] = {
    fm match {
      case Verum => Set()
      case Falsum => Set()
      //case a: Atom => Set(a)
      case x: ExtendedAtom => {
        if (nested) {
          Set(x, x.atom)
        } else {
          Set(x)
        }
      }
      //Wether At, W, Wop are (different encodings) of ExtendedAtoms needs to be tested ...
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
        apply(Wop(wop, fm), nested)
      }
      case Wop(wop, fm1) => {
        fm1 match {
          case Diam(fm2) =>
            if (fm2.isInstanceOf[Atom]) {
              apply(WDiamAtom(wop, fm2.asInstanceOf[Atom]), nested)
            } else {
              apply(fm2, nested)
            }
          case Box(fm2) =>
            if (fm2.isInstanceOf[Atom]) {
              apply(WBoxAtom(wop, fm2.asInstanceOf[Atom]), nested)
            } else {
              apply(fm2, nested)
            }
          case At(u, fm2) =>
            if (fm2.isInstanceOf[Atom]) {
              apply(WAtAtom(wop, u, fm2.asInstanceOf[Atom]), nested)
            } else {
              apply(fm2, nested)
            }
          //
          case x => apply(fm1, nested)
        }
      }
      //... for all other formulas, only the nested expressions need to be tested (if nested==true)
      case Unary(fm1) => apply(fm1, nested)
      case Binary(fm1, fm2) => apply(fm1, nested) union apply(fm2, nested)
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
  def apply[R <: Rule](P: Program[R], sub: Boolean): Set[ExtendedAtom] = {
    apply(P.rules, sub)
  }
}