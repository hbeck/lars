package lars.exp

import lars.exp.Streams.{Timeline, Stream}
import scala.math.{floor,max,min,round}

/**
 * Created by hb on 1/2/15.
 */
object WindowFunctions {

  abstract class WindowFunction() extends Function3[Stream,Int,Seq[Int],Stream]

  val TimeBasedWindow = new WindowFunction() {

    def apply(S: Stream, t: Int, x: Seq[Int]) = {

      val l = x apply 0
      val u = x apply 1
      val d = x apply 2

      val tMin = S.T.lower
      val tMax = S.T.upper

      val td = (round(floor(t / d)) * d).toInt
      val tl = max(tMin, td - l)
      val tu = min(td + u, tMax)

      val T1 = Timeline(tl, tu)

      Stream(T1, S.v|T1)
    }
  }

  }
