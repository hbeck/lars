package lars.core.semantics.programs.extatoms

import lars.core.semantics.formulas._

/**
 * Created by hb on 7/14/15.
 */
case class WBoxAtom(w: WindowOperatorFixedParams, a: Atom) extends Unary(Box(a)) /* Wop(w,Box(a)) */ with ExtendedAtom {
  override def toString = w + "☐" + a
  override def atom = a
}
//object WBoxAtom {
//  def apply(x:WopFm): WBoxAtom = {
//    val boxAtom = x.fm.asInstanceOf[Box]
//    WBoxAtom(x.wop,boxAtom.fm.asInstanceOf[Atom])
//  }
//}