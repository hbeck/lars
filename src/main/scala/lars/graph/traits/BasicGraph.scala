package lars.graph.traits

/**
 * Created by hb on 7/17/15.
 */
trait BasicGraph[V] extends AdjList[V] with Outgoing[V] with IsLeaf[V]

