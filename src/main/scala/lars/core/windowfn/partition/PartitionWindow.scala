package lars.core.windowfn.partition

import lars.core.semantics.formulas.Atom
import lars.core.semantics.streams.{Evaluation, S, Timeline}
import lars.core.windowfn.tuple.{TupleWindowParameters, TupleWindow}
import lars.core.windowfn.WindowFunction

import scala.collection.immutable.HashMap

/**
 * Created by et on 7/13/15.
 */
object PartitionWindow extends WindowFunction[PartitionWindowParameters] {

  override def apply(s: S, t: Int, x: PartitionWindowParameters): S = {

    res(s,t,TupleWindowParameters(x.l,x.u),x.a)
  }

  def res(s:S, t:Int, x:TupleWindowParameters, a:Set[Atom]):S = {
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
  }

  override def fix(x: PartitionWindowParameters) = PartitionWindowFixedParams(x)
  def fix(a:Set[Atom], l:Int, u:Int=0) = PartitionWindowFixedParams(PartitionWindowParameters(a,l,u))
}
