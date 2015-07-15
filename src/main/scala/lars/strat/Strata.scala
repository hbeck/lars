package lars.strat

import lars.core.semantics.programs.extatoms.ExtendedAtoms
import lars.core.semantics.programs.{Program, Rule}

/**
 * Created by hb on 7/13/15.
 */
object Strata {

  def apply[R <: Rule, Pr <: Program[R]](P: Pr): Map[Int,Pr] = {
    val optStrat: Option[Stratification] = Stratification(P)
    if (optStrat.isEmpty) {
      return Map(0 -> P)
    }
    val strat = optStrat.get
    val mMap = new collection.mutable.HashMap[Int,Pr]()
    var nr = -1;
    for (i <- 0 to strat.maxStratum) {
      val rules = P.rules.filter(rule => strat(ExtendedAtoms(rule.head,false).head) == i) //brave; in use case, rule head should be single ExtendedAtom
      if (!rules.isEmpty) {
        nr += 1
        mMap(nr)=P(rules).asInstanceOf[Pr]
      }
    }
    mMap.toMap
  }

}