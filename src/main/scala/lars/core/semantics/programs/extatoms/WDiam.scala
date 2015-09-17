package lars.core.semantics.programs.extatoms

import lars.core.semantics.formulas._

/**
 * Created by hb on 7/14/15.
 */
class WDiam(override val wop: WindowOperatorFixedParams, val a: Atom) extends WindowAtom(wop,Diam(a)) with ExtendedAtom {
  override def toString = wop + "◇" + a
  override def atom = a
}
object WDiam {
  def apply(w: WindowOperatorFixedParams, a: Atom) = new WDiam(w,a)
}