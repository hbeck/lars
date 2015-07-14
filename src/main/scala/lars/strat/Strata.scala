package lars.strat

import lars.core.semantics.programs.Program

/**
 * Created by hb on 7/13/15.
 */
object Strata {

  def apply(P: Program): Map[Int,Program] = {
    val optStrat: Option[Stratification] = Stratification(P)
    if (optStrat.isEmpty) {
      return Map(0 -> P)
    }
    val strat = optStrat.get
    val mMap = new collection.mutable.HashMap[Int,Program]()
    var nr = -1;
    for (i <- 0 to strat.maxStratum) {
      val rules = P.rules.filter(rule => strat(ExtendedAtoms(rule.head,false).head) == i) //brave; in use case, rule head should be single ExtendedAtom
      if (!rules.isEmpty) {
        nr += 1
        mMap(nr)=Program(rules)
      }
    }
    mMap.toMap
  }

}