package lars.core.semantics.programs.extatoms

import lars.core.semantics.formulas._

/**
 * Created by hb on 7/14/15.
 */
class WAt(override val w: WindowOperatorFixedParams, val t:Int, val a: Atom) extends WindowAtom(w,At(t,a)) /* Wop(w,At(t,a)) */ with ExtendedAtom {
  override def toString = w + "@{" + t+ "}"+a
  override def atom = a
  override def nested = Set[ExtendedAtom](this,AtAtom(t,a),a)
}
object WAt {
  def apply(w: WindowOperatorFixedParams, t:Int, a: Atom) = new WAt(w,t,a)
}