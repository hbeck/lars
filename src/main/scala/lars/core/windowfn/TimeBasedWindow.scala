package lars.core.windowfn

import java.lang.Math.{floor, max, min, round}
import lars.core.LStreams.{LStream, Timeline}

/**
 * Created by hb on 5/18/15.
 */

//l: to the left (past), u: to the right (future), d: hopsize
case class TimeBasedWindowParameters(l:Int, u:Int=0, d:Int=1) extends WindowParameters

object TimeBasedWindow extends WindowFunction[TimeBasedWindowParameters]() {

  override def apply(S: LStream, t: Int, x: TimeBasedWindowParameters) = {

    assert(x.d > 0)

    val tMin = S.T.lower
    val tMax = S.T.upper

    val td = (round(floor(t / x.d)) * x.d).toInt
    val tl = max(tMin, td - x.l)
    val tu = min(td + x.u, tMax)

    val T1 = Timeline(tl, tu)

    LStream(T1, S.v|T1)
  }

  def apply(S: LStream, t: Int, x: Int*) : LStream = {
    val l = x apply 0
    val u = if (x.length >= 2) x apply 1 else 0
    val d = if (x.length == 3) x apply 2 else 1
    apply(S,t,TimeBasedWindowParameters(l,u,d))
  }

  def apply(S: LStream, t: Int, x: (Int,Int,Int)) : LStream = apply(S,t,x._1,x._2,x._3)

}
