package lars.core.semantics.programs.extatoms

import lars.core.semantics.formulas.{At, Atom, ExtendedAtom}

/**
 * Created by hb on 7/14/15.
 */
class AtAtom(override val t: Int, val a: Atom) extends At(t,a) with ExtendedAtom {
  override def toString = "@{"+t+"}"+a
  override def atom = a
}
object AtAtom {
  def apply(t: Int, a: Atom) = new AtAtom(t,a)
}
