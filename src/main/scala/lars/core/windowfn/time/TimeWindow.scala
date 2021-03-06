package lars.core.windowfn.time

import java.lang.Math._

import lars.core.semantics.formulas.WindowOperatorFixedParams
import lars.core.semantics.formulas.WindowOperators.ch1
import lars.core.semantics.streams.{S, Timeline}
import lars.core.windowfn.WindowFunction

/**
 * Created by hb on 5/26/15.
 */
object TimeWindow extends WindowFunction[TimeWindowParameters] {

  override def apply(s: S, t: Int, x: TimeWindowParameters): S = {

    assert(x.d > 0)

    val tMin = s.T.lower
    val tMax = s.T.upper

    val td = (round(floor(t / x.d)) * x.d).toInt
    val tl = max(tMin, td - x.l)
    val tu = min(td + x.u, tMax)

    val T1 = Timeline(tl, tu)

    S(T1, s.v|T1)
  }

  def apply(S: S, t: Int, xs: Int*) : S = {
    val x = TimeWindowParameters.from(xs)
    apply(S,t,x)
  }

  def apply(S: S, t: Int, x: (Int,Int,Int)) : S = apply(S,t,x._1,x._2,x._3)

  def fix(x: TimeWindowParameters) = TimeWindowFixedParams(x)
  //def fix(xs: Int*) = TimeBasedWindow(TimeBasedWindowParameters.from(xs))
  def fix(xs: Seq[Int]) = TimeWindowFixedParams(TimeWindowParameters.from(xs))

  //sloppy
  def toOp1(x:Int) = WindowOperatorFixedParams(fix(Seq(x,0,1)),ch1)
  def toOp(x:Int) = WindowOperatorFixedParams(fix(Seq(x,0,1)))
  def toOp1(xs: Int*) = WindowOperatorFixedParams(fix(xs),ch1)
  def toOp(xs: Int*) = WindowOperatorFixedParams(fix(xs))

}