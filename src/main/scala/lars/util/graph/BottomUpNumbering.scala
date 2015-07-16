package lars.util.graph

/**
 * Created by hb on 7/16/15.
 */
object BottomUpNumbering {

  def apply[V](g: InOutGraph[V], min:Int=0): Map[V, Int] = {

    var nr = Map[V,Int]()

    //may be done more efficiently by preordering nodes accordingly instead of naive queuing
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

  private def maxChildNr[V](node: V, g: InOutGraph[V], nr:Map[V,Int]) : Option[Int] = {
    val children = g.incoming(node)
    if (children.exists( g => !nr.contains(g) )) {
      return None
    }
    Option(children.map(nr).reduce(math.max))
  }
}
