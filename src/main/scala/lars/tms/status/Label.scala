package lars.tms.status

import lars.core.ClosedIntInterval

/**
 * Created by hb on 7/14/15.
 */
case class Label(var status: Status, val intervals:collection.mutable.Set[ClosedIntInterval]=new collection.mutable.HashSet[ClosedIntInterval])

object Label {

  def apply(status: Status, pairs:(Int,Int)*): Label = {
    val set = new collection.mutable.HashSet[ClosedIntInterval]()
    for (pair <- pairs) {
      set += new ClosedIntInterval(pair._1,pair._2)
    }
    Label(status,set)
  }
}
