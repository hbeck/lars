package lars.core.semantics.programs.extatoms

import lars.core.semantics.formulas._

/**
 * Created by hb on 7/14/15.
 */
class WDiam(override val w: WindowOperatorFixedParams, val a: Atom) extends WindowAtom(w,Diam(a)) with ExtendedAtom {
  override def toString = w + "◇" + a
  override def atom = a
}
object WDiam {
  def apply(w: WindowOperatorFixedParams, a: Atom) = new WDiam(w,a)
}