package lars.core.semantics

/**
 * Created by hb on 7/15/15.
 */
package object formulas {

  def not(fm: Formula) = Not(fm)
  def box(fm: Formula) = Box(fm)
  def diamond(fm: Formula) = Diam(fm)

}
