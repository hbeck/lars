package lars.core.semantics

/**
 * Created by hb on 7/15/15.
 */
package object formulas {

  def not(fm: Formula) = Not(fm)
  def -(fm: Formula) = Not(fm)
  def B(fm: Formula) = Box(fm)
  def D(fm: Formula) = Diam(fm)

}
