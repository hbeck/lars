package lars.core

import lars.core.LStreams.{Timeline,LStream}
import scala.math.{floor,max,min,round}

/**
 * Created by hb on 1/2/15.
 */
object WindowFunctions {

  //abstract class WindowType
  //abstract class TimeBasedWindowType extends WindowType

  abstract class WindowParameters

  abstract class WindowFunction[U <: WindowParameters]() extends ((LStream, Int, U) => LStream)

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

  }

}
