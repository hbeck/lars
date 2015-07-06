package lars.core.semantics.formulas

/**
 * Created by hb on 1/2/15.
 */

object FormulaFunctions {

  def not(fm: Formula) = Not(fm)
  def box(fm: Formula) = Box(fm)
  def diamond(fm: Formula) = Diam(fm)
  def at = (t:Int) => ((fm: Formula) => At(t,fm))

}

