package lars.graph.alg

import lars.graph.traits.BasicGraph

/**
 * Created by hb on 7/16/15.
 */
object BottomUpNumbering {

  def apply[V, G <: BasicGraph[V]](g: G, min:Int=0): Map[V, Int] = {

    var nr = Map[V,Int]()

    //may be done more efficiently by pre-ordering nodes accordingly instead of naive queuing
    var queue = collection.mutable.Queue[V]()
    queue ++= g.nodes

    while (!queue.isEmpty) {
      val node: V = queue.dequeue()
      if (g.isLeaf(node)) {
        nr = nr + (node -> min)
      } else {
        val maxChild:Option[Int] = maxChildNr(node,g,nr)
        if (maxChild.isDefined) {
          val n = maxChild.get + 1
          nr = nr + (node -> n)
        } else {
          queue.enqueue(node)
        }
      }
    }
    nr
  }

  private def maxChildNr[V,G <: BasicGraph[V]](node: V, g: G, nr:Map[V,Int]) : Option[Int] = {
    val children = g.outgoing(node)
    if (children.exists( g => !nr.contains(g) )) {
      return None
    }
    Option(children.map(nr).reduce(math.max))
  }
}
