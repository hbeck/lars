package lars.core.semantics.programs.general.inspect

import lars.core.semantics.formulas.{Box, Diam, Formula}

/**
 * Created by hb on 8/5/15.
 */
object IsTemporallyQuantified {

  def apply(fm: Formula): Boolean = {
    fm match {
      case Diam(fm1) => true
      case Box(fm1) => true
      case _ => false
    }
  }

}
