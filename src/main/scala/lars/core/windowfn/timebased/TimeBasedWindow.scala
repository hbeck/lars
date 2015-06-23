package lars.core.windowfn.timebased

import lars.core.semantics.formulas.FormulaFunctions._
import java.lang.Math._

import lars.core.semantics.streams.{S, Timeline}
import lars.core.windowfn.WindowFunction

/**
 * Created by hb on 5/26/15.
 */
object TimeBasedWindow extends WindowFunction[TimeBasedWindowParameters]() {

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

  def apply(S: S, t: Int, x: Int*) : S = {
    val l = x apply 0
    val u = if (x.length >= 2) x apply 1 else 0
    val d = if (x.length == 3) x apply 2 else 1
    apply(S,t,TimeBasedWindowParameters(l,u,d))
  }

  def apply(S: S, t: Int, x: (Int,Int,Int)) : S = apply(S,t,x._1,x._2,x._3)

  //sloppy
  def toOp(x:Int) = win(this, TimeBasedWindowParameters(x))
  def toOp(x:(Int,Int,Int)) = win(this, TimeBasedWindowParameters(x._1,x._2,x._3))

}
