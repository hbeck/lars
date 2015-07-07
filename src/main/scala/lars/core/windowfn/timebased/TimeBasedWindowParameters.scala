package lars.core.windowfn.timebased

import lars.core.windowfn.WindowParameters

/**
 * Created by hb on 5/26/15.
 */
//l: to the left (past), u: to the right (future), d: hopsize
case class TimeBasedWindowParameters(l:Int, u:Int=0, d:Int=1) extends WindowParameters {
  override def toString = {
    if (l != 0) {
      if (u == 0 && d == 1)
        ""+l
      else if (u!=0 && d == 1)
        l + "+" + u
      else
        "(" + l + "," + u + "," + d + ")"
    } else {
      if (u != 0 && d == 1)
        "+"+u
      else
        "(" + l + "," + u + "," + d + ")"
    }
  }
}

object TimeBasedWindowParameters {

  def from(xs: Seq[Int]) : TimeBasedWindowParameters = {
    val l = xs apply 0
    val u = if (xs.length >= 2) xs apply 1 else 0
    val d = if (xs.length == 3) xs apply 2 else 1
    TimeBasedWindowParameters(l,u,d)
  }

//  def from(xs: List[Int]) : TimeBasedWindowParameters = {
//    val l = xs apply 0
//    val u = if (xs.length >= 2) xs apply 1 else 0
//    val d = if (xs.length == 3) xs apply 2 else 1
//    TimeBasedWindowParameters(l,u,d)
//  }

  //def from(x: (Int,Int,Int)) : TimeBasedWindowParameters= TimeBasedWindowParameters(x._1,x._2,x._3)

}
