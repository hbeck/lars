package lars.graph.traits

import lars.graph.Graph

/**
 * Created by hb on 7/17/15.
 */
trait Outgoing[V] extends Graph[V] {

  def outgoing(v:V): Set[V]

}
