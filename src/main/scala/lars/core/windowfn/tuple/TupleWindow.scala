package lars.core.windowfn.tuple

import java.lang.Math._

import lars.core.semantics.streams.{Timeline, S}
import lars.core.windowfn.{WindowFunctionFixedParams, WindowFunction}

/**
 * Created by et on 7/13/15.
 */
object TupleWindow extends WindowFunction[TupleWindowParameters]{



  override def apply(s: S, t: Int, x: TupleWindowParameters): S = {

    val tMin = s.T.lower
    val tMax = s.T.upper

    /*val tl = max(tMin, td - x.l)
    val tu = min(td + x.u, tMax)

    val T1 = Timeline(tl, tu)

    S(T1, s.v|T1)*/
  }

  override def fix(x: TupleWindowParameters): WindowFunctionFixedParams = ???
}
