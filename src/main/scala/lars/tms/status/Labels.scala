package lars.tms.status
import lars.core.semantics.formulas.ExtendedAtom



/**
 * Created by hb on 7/14/15.
 */
case class Labels(val labelOf: collection.mutable.Map[ExtendedAtom,Label]) {

  def label(x:ExtendedAtom) = labelOf(x)

  def status(x: ExtendedAtom) = labelOf(x).status

  def intervals(x: ExtendedAtom) = labelOf(x).intervals

}
