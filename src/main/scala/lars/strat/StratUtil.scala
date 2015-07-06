package lars.strat

import lars.core.semantics.formulas._
import lars.core.semantics.programs.{Program, Rule}

import scala.annotation.tailrec

/**
 * Created by hb on 7/6/15.
 */
object StratUtil {

  def extendedAtoms(fm: Formula) : Set[ExtendedAtom] = {
    fm match {
      case a: Atom           => Set(a)
      case Not(fm1)          => extendedAtoms(fm1)
      case And(fm1, fm2)     => extendedAtoms(fm1) union extendedAtoms(fm2)
      case Or(fm1, fm2)      => extendedAtoms(fm1) union extendedAtoms(fm2)
      case Implies(fm1, fm2) => extendedAtoms(fm1) union extendedAtoms(fm2)
      case Diam(fm1)         => extendedAtoms(fm1)
      case Box(fm1)          => extendedAtoms(fm1)
      case At(u,fm1)         => if (fm1.isInstanceOf[Atom]) {
                                  val a = fm1.asInstanceOf[Atom]
                                  Set(a, AtAtom(u,a))
                                }
                                else {
                                  extendedAtoms(fm1)
                                }
      case W(wop,fm1)        => {
        fm1 match {
          case DiamAtom(a) => Set(WDiamAtom(wop,DiamAtom(a)),a)
          case BoxAtom(a) => Set(WBoxAtom(wop,BoxAtom(a)),a)
          case AtAtom(u,a) => Set(WAtAtom(wop,AtAtom(u,a)),AtAtom(u,a),a)
          //
          case Diam(fm1) => if (fm1.isInstanceOf[Atom]) {
                              val a = fm1.asInstanceOf[Atom]
                              extendedAtoms(WDiamAtom(wop,DiamAtom(a)))
                            } else {
                              extendedAtoms(fm1)
                            }
          case Box(fm1) => if (fm1.isInstanceOf[Atom]) {
                              val a = fm1.asInstanceOf[Atom]
                              extendedAtoms(WBoxAtom(wop,BoxAtom(a)))
                           } else {
                              extendedAtoms(fm1)
                           }
          case At(u,fm1) => if (fm1.isInstanceOf[Atom]) {
                              val a = fm1.asInstanceOf[Atom]
                              extendedAtoms(WAtAtom(wop,AtAtom(u,a)))
                            } else {
                              extendedAtoms(fm1)
                            }
          case x => extendedAtoms(fm1)
        }
      }
      //for extended atoms
      case DiamAtom(a)       => Set(a)
      case BoxAtom(a)        => Set(a)
      //extended atoms
      case AtAtom(u,a)       => Set(AtAtom(u,a),a)
      case WAtAtom(wop,aa)   => Set(WAtAtom(wop,aa)) ++ extendedAtoms(aa)
      case WDiamAtom(wop,da) => Set(WDiamAtom(wop,da)) ++ extendedAtoms(da)
      case WBoxAtom(wop,ba)  => Set(WBoxAtom(wop,ba)) ++ extendedAtoms(ba)
    }
  }

  @tailrec
  def extendedAtoms(rules: Set[Rule], result: Set[ExtendedAtom]) : Set[ExtendedAtom] = {
    if (rules isEmpty) {
      return result
    }
    val rule = rules.head
    val curr = extendedAtoms(rule.head) ++ extendedAtoms(rule.body)
    extendedAtoms(rules.tail, result ++ curr)
  }

  def extendedAtoms(P: Program): Set[ExtendedAtom] = {
    extendedAtoms(P.rules, Set[ExtendedAtom]())
  }

}
