package lars.core.semantics.programs.extatoms

import lars.core.semantics.formulas._

/**
 * Created by hb on 7/14/15.
 */
class WBox(override val w: WindowOperatorFixedParams, val a: Atom) extends WindowAtom(w,Box(a)) with ExtendedAtom {
  override def toString = w + "☐" + a
  override def atom = a
}
object WBox {
  def apply(w:WindowOperatorFixedParams, a: Atom) = new WBox(w,a)
}