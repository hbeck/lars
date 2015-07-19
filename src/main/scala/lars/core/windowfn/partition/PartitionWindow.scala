package lars.core.windowfn.partition

import lars.core.semantics.formulas.Atom
import lars.core.semantics.streams.{Evaluation, S, Timeline}
import lars.core.windowfn.tuple.{TupleWindowFixedParams, TupleWindowParameters, TupleWindow}
import lars.core.windowfn.WindowFunction

import scala.collection.immutable.HashMap

/**
 * Created by et on 7/13/15.
 */
object PartitionWindow extends WindowFunction[PartitionWindowParameters] {

  override def apply(s: S, t: Int, x: PartitionWindowParameters): S = {

//    PartitionWindowParameters(idx: Atom => Int, n: Int => TupleWindowFixedParams)
    val subS = s.partition(x.idx)
    var result = S(s.T)

    for((i,su) <- subS){
      result = result ++ TupleWindow(su,t,TupleWindowParameters(x.n(i).x.l, x.n(i).x.u))
    }
    result
  }

  /*def res(s:S, t:Int, x:TupleWindowParameters, a:Set[Atom]):S = {
    var wt:S = S(s.T)
    if(a.size>1){
      wt = TupleWindow(idx(s,a.head),t,x) ++ res(s,t,x,a.tail)
    } else if(a.size == 1){
      wt = TupleWindow(idx(s,a.head),t,x)
    }
    wt
  }

  def idx(s:S, a:Atom) : S = {
    var subS = S(s.T)
    for((i,j) <- s.v.mapping){
      for(k <- j) {
        if (a == k) {
          subS + (i,k)
        }
      }
    }
    subS
  }*/

  override def fix(x: PartitionWindowParameters) = PartitionWindowFixedParams(x)
  def fix(idx: Atom => Int, n: Int => TupleWindowFixedParams) = PartitionWindowFixedParams(PartitionWindowParameters(idx,n))
}
