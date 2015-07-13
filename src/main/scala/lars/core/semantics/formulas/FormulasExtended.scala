package lars.core.semantics.formulas

import lars.core.semantics.formulas.WindowOperators.{StreamChoice, ch2}
import lars.core.windowfn.WindowFunctionFixedParams

//TODO rethink this dual ontology. especially At(u,a) == AtAtom(u,a) etc...

/**
 * Created by hb on 7/10/15.
 */
case class WindowOperatorFixedParams(wfn: WindowFunctionFixedParams, ch:StreamChoice=ch2) {
  override def toString = "⊞^{"+wfn+"}"
}

case class DiamAtom(a:Atom) extends Formula {
  override def toString = "◇"+a
}
case class BoxAtom(a:Atom) extends Formula {
  override def toString = "☐"+a
}
case class AtAtom(t: Int, a: Atom) extends ExtendedAtom {
  override def toString = "@{"+t+"}"+a
}
case class WAtAtom(w: WindowOperatorFixedParams, aa: AtAtom) extends ExtendedAtom {
  override def toString = w + "" + aa
}
case class WDiamAtom(w: WindowOperatorFixedParams, da: DiamAtom) extends ExtendedAtom {
  override def toString = w + "" + da
}
case class WBoxAtom(w: WindowOperatorFixedParams, ba: BoxAtom) extends ExtendedAtom {
  override def toString = w + "" + ba
}
