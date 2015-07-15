package lars.core.semantics.programs.extatoms

import lars.core.semantics.formulas._

/**
 * Created by hb on 7/14/15.
 */
class WDiam(override val w: WindowOperatorFixedParams, val a: Atom) extends WindowAtom(w,Diam(a)) /* Wop(w,Diam(a)) */ with ExtendedAtom {
  override def toString = w + "◇" + a
  override def atom = a
}
//TODO WDiam(w,a) vs WAtom(w,Diam(a)) vs WAtom(DiamAtom(a)) s.t. DiamAtom extends some interface that indicate it does not nest a formula...
object WDiam {
  def apply(w: WindowOperatorFixedParams, a: Atom) = new WDiam(w,a)
}