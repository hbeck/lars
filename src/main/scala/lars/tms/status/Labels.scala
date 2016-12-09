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

  def intervals(x: ExtendedAtom): collection.immutable.Set[ClosedIntInterval] = label(x).intervals

  def update(x: ExtendedAtom, l:Label): Unit = labelOf(x)=l

  def addInterval(x: ExtendedAtom, interval: ClosedIntInterval): Unit = update(x, new Label(status(x),intervals(x)++Set(interval)))

  def labels = labelOf

  def copy: Labels = {
    var r = collection.mutable.Map[ExtendedAtom,Label]()
    for((k,v) <- labelOf) {
      r += k -> v
    }
    Labels(r)
  }

  override def toString: String = {
    var str = " "
    for((a,l) <- labelOf){
      str += a+" -> "+l+"\n"
      str += "\t"
    }
    str
  }

  //add convenience method like updateStatus etc whatever needed

}


