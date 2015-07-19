package lars.core.semantics.programs.extatoms

import lars.core.semantics.formulas._

/**
 * Created by hb on 7/14/15.
 *
 * used to determine extended atoms automatically by the property of being an extended atom
 * (provides reference to nested atoms)
 */
abstract class WindowAtom(val w: WindowOperatorFixedParams, override val fm: Formula) extends WindowFormula(w,fm) with ExtendedAtom
