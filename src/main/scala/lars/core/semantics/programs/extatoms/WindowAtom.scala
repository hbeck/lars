package lars.core.semantics.programs.extatoms

import lars.core.semantics.formulas.{ExtendedAtom, WindowOperatorFixedParams, Formula, Unary}

/**
 * Created by hb on 7/14/15.
 */
abstract class WindowAtom(w: WindowOperatorFixedParams, fm: Formula) extends Unary(fm) with ExtendedAtom {}
