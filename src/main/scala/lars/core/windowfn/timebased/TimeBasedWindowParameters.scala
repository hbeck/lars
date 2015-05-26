package lars.core.windowfn.timebased

import lars.core.windowfn.WindowParameters

/**
 * Created by hb on 5/26/15.
 */
//l: to the left (past), u: to the right (future), d: hopsize
case class TimeBasedWindowParameters(l:Int, u:Int=0, d:Int=1) extends WindowParameters
