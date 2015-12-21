package lars.graph.quotient

import scala.collection.mutable.HashSet

/**
 * Used to make explicit the set of nodes of a partition.
 *
 * Created by hb on 8/11/15.
 *
 * //immutable would be better; this one's written faster...
 */
class Block[V] extends HashSet[V]

object Block {
  def apply[V](seq:Set[V]): Block[V] = {
    val block = new Block[V]()
    block ++= seq
  }
}
