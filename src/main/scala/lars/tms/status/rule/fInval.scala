package lars.tms.status.rule

import lars.core.semantics.programs.standard.StdRule
import lars.tms.status.Labels
import lars.tms.status.Status.{in, out, unknown}

/**
 * Created by hb on 7/14/15.
 */
object fInval {

  def apply(L: Labels, r:StdRule): Boolean = {
/*    println("lableH: "+L.status(r.h))
    r.B.foreach(k => println("lable of "+k+": "+L.status(k)))
    println("r: "+r)
    println("first part: "+r.B.forall(L.status(_) != unknown))
    println("second part: "+r.Bp.exists(L.status(_) == out))
    println("third part: "+r.Bn.exists(L.status(_) == in))*/
    r.B.forall(L.status(_) != unknown) &&
      (r.Bp.exists(L.status(_) == out) || r.Bn.exists(L.status(_) == in))
  }

}
