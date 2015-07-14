package lars.tms.status

import lars.core.ClosedIntInterval
import lars.core.semantics.formulas.ExtendedAtom



/**
 * Created by hb on 7/14/15.
 */
case class Labels(val labelOf: collection.mutable.Map[ExtendedAtom,Label]) {

  def label(x:ExtendedAtom): Label = labelOf(x)

  def status(x: ExtendedAtom): Status = labelOf(x).status

  def intervals(x: ExtendedAtom): collection.mutable.Set[ClosedIntInterval] = labelOf(x).intervals

}
