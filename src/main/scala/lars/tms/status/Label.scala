package lars.tms.status

import lars.core.ClosedIntInterval
import collection.immutable._

/**
 * Created by hb on 7/14/15.
 */
case class Label(var status: Status, intervals:Set[ClosedIntInterval]=HashSet[ClosedIntInterval]())

object Label {

  def apply(status: Status, pairs:(Int,Int)*): Label = {
    var set = new HashSet[ClosedIntInterval]()
    for (pair <- pairs) {
      set ++= HashSet(new ClosedIntInterval(pair._1,pair._2))
    }
    Label(status,set)
  }
}
