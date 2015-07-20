package lars.core.windowfn.partition

import lars.core.semantics.formulas.Atom
import lars.core.semantics.streams.{Evaluation, S, Timeline}
import lars.core.windowfn.tuple.{TupleWindowFixedParams, TupleWindowParameters, TupleWindow}
import lars.core.windowfn.WindowFunction

import scala.collection.immutable.{Map, HashMap}

/**
 * Created by et on 7/13/15.
 */
object PartitionWindow extends WindowFunction[PartitionWindowParameters] {

  override def apply(s: S, t: Int, x: PartitionWindowParameters): S = {

    val subS = partition(x.idx,s)
    var result = S(s.T)

    for ((i,su) <- subS) {
      result = result ++ TupleWindow(su,t,TupleWindowParameters(x.n(i).x.l, x.n(i).x.u))
    }

    for ((k,v) <- result.v.mapping) {
      if (v.isEmpty) {
        result = S(s.T,Evaluation(result.v.mapping - k))
      }
    }
    result
  }

  def partition(idx: Atom => Int, s:S): Map[Int, S] = {
    val m = new collection.mutable.HashMap[Int, S]

    for ((t,as) <- s.v.mapping) {
      for (a <- as) {
        if (m.contains(idx(a))) {
         m(idx(a)) =  m(idx(a)) + (t->a)
        } else {

          m += (idx(a) -> (S(s.T) + (t->a)))
        }
      }
    }
    m.toMap
  }

  override def fix(x: PartitionWindowParameters) = PartitionWindowFixedParams(x)
  def fix(idx: Atom => Int, n: Int => TupleWindowFixedParams) = PartitionWindowFixedParams(PartitionWindowParameters(idx,n))
}
