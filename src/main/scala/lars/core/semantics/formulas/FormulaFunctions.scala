package lars.core.semantics.formulas

import lars.core.semantics.formulas.WindowOperators.{ch1, ch2, StreamChoice}
import lars.core.windowfn.{WindowFunction, WindowParameters}

/**
 * Created by hb on 1/2/15.
 */

object FormulaFunctions {

  def not(fm: Formula) = Not(fm)
  def box(fm: Formula) = Box(fm)
  def diamond(fm: Formula) = Diam(fm)
  def at = (t:Int) => ((fm: Formula) => At(t,fm))
  //general window, with choice function as parameter
  def gwin[X <: WindowParameters] = (w:WindowFunction[X], ch: StreamChoice, x: X) => ((fm: Formula) => W(WindowOperator(w,ch,x),fm))
  //typical window with choice2
  def win[X <: WindowParameters] = (w:WindowFunction[X], x:X) => ((fm:Formula) => W(WindowOperator(w,ch2,x),fm))
  //window with choice 1
  def win1[X <: WindowParameters] = (w:WindowFunction[X], x:X) => ((fm:Formula) => W(WindowOperator(w,ch1,x),fm))

}

