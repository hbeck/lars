package lars.core.semantics.formulas

import WindowOperators.StreamChoice
import lars.core.windowfn.{WindowFunction, WindowParameters}

/**
 * Created by hb on 1/2/15.
 */

object FormulaFunctions {

  def not(fm: Formula) = Not(fm)
  def box(fm: Formula) = B(fm)
  def diamond(fm: Formula) = D(fm)
  def at = (t:Int) => ((fm: Formula) => At(t,fm))
  def win[X <: WindowParameters] = (w:WindowFunction[X], ch: StreamChoice, x: X) => ((fm: Formula) => W(w,ch,x,fm))

}

