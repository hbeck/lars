package lars.graph.traits

/**
  * Created by hb on 7/17/15.
  */
trait Incoming[V] {

   def incoming(v:V): Set[V]

 }
