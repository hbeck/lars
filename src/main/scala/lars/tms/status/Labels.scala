package lars.tms.status

import lars.core.ClosedIntInterval
import lars.core.semantics.formulas.ExtendedAtom
import lars.tms.status.Status.unknown


/**
 * Created by hb on 7/14/15.
 */
case class Labels(private val labelOf: collection.mutable.Map[ExtendedAtom,Label]=collection.mutable.Map[ExtendedAtom,Label]()) {

  def label(x:ExtendedAtom): Label = labelOf.getOrElse(x,Label(unknown))

  def status(x: ExtendedAtom): Status = label(x).status

  def intervals(x: ExtendedAtom): collection.mutable.Set[ClosedIntInterval] = label(x).intervals

  def update(x: ExtendedAtom, l:Label): Unit = {
    labelOf(x)=l
  }

  //add convenience method like updateStatus etc whatever needed

}


