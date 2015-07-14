package lars.core.semantics.programs.extatoms

import lars.core.semantics.formulas.{Atom, ExtendedAtom, Unary}

/**
 * Created by hb on 7/14/15.
 */
case class AtAtom(t: Int, a: Atom) extends Unary(a) /* At(t,a) */ with ExtendedAtom {
  override def toString = "@{"+t+"}"+a
  override def atom = a
}
//object AtAtom {
//  def apply(x:At): AtAtom = {
//    AtAtom(x.t,x.fm.asInstanceOf[Atom])
//  }
//}
