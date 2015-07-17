package lars.graph.traits

/**
 * Created by hb on 7/17/15.
 */
trait Outgoing[V] {

  def outgoing(v:V): Set[V]

}
