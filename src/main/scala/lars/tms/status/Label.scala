package lars.tms.status

import lars.core.ClosedIntInterval

/**
 * Created by hb on 7/14/15.
 */
case class Label(var status: Status, val intervals:collection.mutable.Set[ClosedIntInterval])
