package lars.core.semantics.programs.extatoms

import lars.core.semantics.formulas._

/**
 * Created by hb on 7/14/15.
 */
class WBox(override val wop: WindowOperatorFixedParams, val a: Atom) extends WindowAtom(wop,Box(a)) with ExtendedAtom {
  override def toString = wop + "☐" + a
  override def atom = a
}
object WBox {
  def apply(w:WindowOperatorFixedParams, a: Atom) = new WBox(w,a)
}