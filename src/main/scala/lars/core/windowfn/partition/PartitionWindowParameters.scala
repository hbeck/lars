package lars.core.windowfn.partition

import lars.core.semantics.formulas.Atom
import lars.core.windowfn.WindowParameters
import lars.core.windowfn.tuple.TupleWindowFixedParams

/**
 * Created by et on 7/13/15.
 */

case class PartitionWindowParameters(idx: Atom => Int, n: Int => TupleWindowFixedParams) extends WindowParameters {

}
/*

object PartitionWindowParameters {
  def from(as: Set[Atom], xs: Seq[Int]) : PartitionWindowParameters = {
    /*val a = as
    val l = xs.head
    val u = if (xs.length == 2) xs(1) else 0
    PartitionWindowParameters(a, l,u)*/
    null
  }
}*/
