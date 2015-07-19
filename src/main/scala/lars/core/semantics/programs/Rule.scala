package lars.core.semantics.programs

import lars.core.semantics.formulas.Formula

/**
 * Created by hb on 7/14/15.
 */
abstract class Rule {

  def head: Formula

  def body: Formula

  def asFormula = body implies head

}
