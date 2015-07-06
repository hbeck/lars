package lars.core.windowfn.timebased

import java.lang.Math._

import lars.core.semantics.formulas.{WindowOperator1, WindowOperator2}
import lars.core.semantics.streams.{S, Timeline}
import lars.core.windowfn.WindowFunctionParam

/**
 * Created by hb on 5/26/15.
 */
object TimeBasedWindowParam extends WindowFunctionParam[TimeBasedWindowParameters] {

  override def apply(s: S, t: Int, x: TimeBasedWindowParameters): S = {

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
    val x = TimeBasedWindowParameters.from(xs)
    apply(S,t,x)
  }

  def apply(S: S, t: Int, x: (Int,Int,Int)) : S = apply(S,t,x._1,x._2,x._3)

  def fix(x: TimeBasedWindowParameters) = TimeBasedWindow(x)
  //def fix(xs: Int*) = TimeBasedWindow(TimeBasedWindowParameters.from(xs))
  def fix(xs: Seq[Int]) = TimeBasedWindow(TimeBasedWindowParameters.from(xs))

  //sloppy
  def toOp1(x:Int) = WindowOperator1(fix(Seq(x,0,1)))
  def toOp2(x:Int) = WindowOperator2(fix(Seq(x,0,1)))
  def toOp1(xs: Int*) = WindowOperator1(fix(xs))
  def toOp2(xs: Int*) = WindowOperator2(fix(xs))

}