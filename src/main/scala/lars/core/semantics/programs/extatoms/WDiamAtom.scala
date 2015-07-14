package lars.core.semantics.programs.extatoms

import lars.core.semantics.formulas._

/**
 * Created by hb on 7/14/15.
 */
case class WDiamAtom(w: WindowOperatorFixedParams, a: Atom) extends WindowAtom(w,Diam(a)) /* Wop(w,Diam(a)) */ with ExtendedAtom {
  override def toString = w + "◇" + a
  override def atom = a
}
//object WDiamAtom {
//  def apply(x:WopFm): WDiamAtom = {
//    val diamAtom = x.fm.asInstanceOf[Diam]
//    WDiamAtom(x.wop,diamAtom.fm.asInstanceOf[Atom])
//  }
//}