package lars.core.semantics.programs.extatoms

import lars.core.semantics.formulas._

/**
 * Created by hb on 7/14/15.
 */
case class WAtAtom(w: WindowOperatorFixedParams, t:Int, a: Atom) extends WindowAtom(w,At(t,a)) /* Wop(w,At(t,a)) */ with ExtendedAtom {
  override def toString = w + "@{" + t+ "}"+a
  override def atom = a
  override def nested = Set[ExtendedAtom](this,AtAtom(t,a),a)
}
//object WAtAtom {
//  def apply(x:WopFm): WAtAtom = {
//    val at = x.fm.asInstanceOf[At]
//    WAtAtom(x.wop,at.t,at.fm.asInstanceOf[Atom])
//  }
//}