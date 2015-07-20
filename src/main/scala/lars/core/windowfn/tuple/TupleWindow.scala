package lars.core.windowfn.tuple

import lars.core.semantics.formulas.Atom
import lars.core.semantics.streams.{Evaluation, S, Timeline}
import lars.core.windowfn.WindowFunction

/**
 * Created by et on 7/13/15.
 */
object TupleWindow extends WindowFunction[TupleWindowParameters] {

  override def apply(s: S, t: Int, x: TupleWindowParameters): S = {

    val tMin = s.T.lower
    val tMax = s.T.upper
    val l = x.l
    val u = x.u

    //tuple time bound
    val tlSet = Set(tMin) ++ (tMin to t).filter(tp => (s | Timeline(tp, t)).size >= l)
    val tl = tlSet.reduce(math.max)
    val tuSet = Set(tMax) ++ (t+1 to tMax).filter(tp => (s | Timeline(t+1, tp)).size >= u) //bug in reactknow paper, says (t+1 to tMax)!
    val tu = tuSet.reduce(math.min)

    val Tl = Timeline(tl, t)
    val Tu = Timeline(math.min(t+1, tu), tu) // in the paper it says Timeline(t+1, tu)

    /*check for special cases l==0 and u==0 -> different timeline needed*/
    var Tp = Timeline(tl, tu)

    if (l == 0) {
      Tp = Timeline(t + 1, tu)
    } else if (u == 0) {
      Tp = Timeline(tl, tu-1)
    }

    val mMap = new collection.mutable.HashMap[Int, Set[Atom]]()

    val v = s.v

    var lAlready = 0
    var uAlready = 0
    for (tp <- tl+1 to tu-1) { //former boundaries: tl+1 to tu-1
      /*if (v(tp).nonEmpty) */mMap(tp) = v(tp)

      if (tp <= t) {
        lAlready += v(tp).size
      } else {
        uAlready += v(tp).size
      }
    }
    if (l > 0) {
      if ((s | Tl).size <= l) {
      if(v(tl).nonEmpty)  mMap(tl) = v(tl)
      } else {
        //>l
        val diff = l - lAlready
        val Xl = v(tl).take(diff)
        mMap(tl) = Xl
      }
    }
    if (u > 0) {
      if ((s | Tu).size <= u) {
        mMap(tu) = v(tu)
      } else {
        //>u
        val diff = u - uAlready
        val Xu = v(tu).take(diff)
        mMap(tu) = Xu
      }
    }

    val vp = Evaluation(mMap.toMap)

    S(Tp, vp|Tp)
  }

  override def fix(x: TupleWindowParameters) = TupleWindowFixedParams(x)
  def fix(l:Int, u:Int=0) = TupleWindowFixedParams(TupleWindowParameters(l,u))
}
