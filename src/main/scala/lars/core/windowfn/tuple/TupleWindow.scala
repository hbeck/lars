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
    val tlSet = Set(tMin) ++ (tMin to t).filter( tp => (s|Timeline(tp,t)).size >= l )
    val tl = tlSet.reduce(math.max)
    val tuSet = Set(tMax) ++ (t+1 to tMax).filter( tp => (s|Timeline(t+1,tp)).size >= u)
    val tu = tuSet.reduce(math.min)

    val Tl = Timeline(tl,t)
    val Tu = Timeline(t+1,tu)

    val Tp = Timeline(tl,tu)

    val mMap = new collection.mutable.HashMap[Int,Set[Atom]]()

    val v = s.v
    var lAlready = 0
    var uAlready = 0
    for (tp <- tl+1 to tu-1) {
      mMap(tp)=v(tp)
      if (tp <= t) {
        lAlready += v(tp).size
      } else {
        uAlready += v(tp).size
      }
    }
    if ((s|Tl).size <= l) {
      mMap(tl)=v(tl)
    } else { //>l
      val diff = l - lAlready
      val Xl = v(tl).take(diff)
      mMap(tl)=Xl
    }
    if ((s|Tu).size <= u) {
      mMap(tu)=v(tu)
    } else { //>u
      val diff = u - uAlready
      val Xu = v(tu).take(diff)
      mMap(tu)=Xu
    }

    val vp = Evaluation(mMap.toMap)

    S(Tp, vp|Tp)
  }

  override def fix(x: TupleWindowParameters) = TupleWindowFixedParams(x)
}
