package lars.core.semantics.programs.standard

import lars.core.semantics.programs.Rule
import lars.core.semantics.formulas._
import lars.core.semantics.programs.extatoms.AtAtom

/**
 * Created by hb on 6/23/15.
 */
case class StdRule(h:ExtendedAtom, Bp:Set[ExtendedAtom], Bn:Set[ExtendedAtom]=Set[ExtendedAtom]()) extends Rule {

  assert(h.isInstanceOf[Atom] || h.isInstanceOf[AtAtom])
  
  private val posBodyFm: Formula = And(Bp)
  private val negBodyFm: Formula = And(Bn.map(x => Not(x)))
  private val bodyFm = And(posBodyFm,negBodyFm)

  val B = Bp ++ Bn

  override def head: Formula = h

  override def body: Formula = bodyFm

  override def equals(that: Any) = {
    that match {
      case StdRule(h,pb,nb) => h == h && Bp == pb && Bn == nb
      case _ => false
    }
  }

  override def toString = {
    val sb = new StringBuilder()
    var c = 0;
    for (x <- Bp) {
      c += 1
      if (c>1) {
        sb.append(", ")
      }
      sb.append(x)
    }
    for (x <- Bn) {
      sb.append(", not ").append(x)
    }
    h + " ← " + sb.toString
  }

  def contains(x: ExtendedAtom): Boolean = {
    (h == x) || B.contains(x)
  }
}
