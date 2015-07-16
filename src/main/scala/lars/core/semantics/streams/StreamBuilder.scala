package lars.core.semantics.streams

import lars.core.semantics.formulas.Atom
import lars.util.map.Merge

import scala.collection.immutable.HashMap

/**
 * TODO: builder should me mutable!
 *
 * Created by hb on 7/13/15.
 */
case class StreamBuilder(val map:Map[Int,Set[Atom]]=new HashMap[Int,Set[Atom]]()) {
  
  def +(t:Int, a:Atom): StreamBuilder = {
    if (map.contains(t)) {
      val atoms = map(t)
      val m = map.updated(t,atoms + a)
      return StreamBuilder(m)
    } else {
      val m = map + (t -> Set(a))
      return StreamBuilder(m)
    }
  }
  
  def ++(sb: StreamBuilder): StreamBuilder = {
    val merged: Map[Int, Set[Atom]] = Merge(this.map,sb.map)
    StreamBuilder(merged)
  }
  
  def +(xs: (Int,Atom)*):StreamBuilder = {
    val sbs:Seq[StreamBuilder] = xs.map( x => StreamBuilder(new HashMap[Int,Set[Atom]]() + (x._1 -> Set(x._2))) )
    sbs.reduce((sb1,sb2) => sb1 ++ sb2)
  }
  
  def build(): S = {
    val min = map.keySet.reduce(math.min)
    val max = map.keySet.reduce(math.max)
    val T = Timeline(min,max)
    S(T,Evaluation(map))
  }
  
  def build(lower: Int, upper: Int): S = {
    build(Timeline(lower,upper))
  }
  
  def build(T: Timeline): S = {
    val filteredMap: Map[Int, Set[Atom]] = map.filterKeys(k => (T.lower <= k && k <= T.upper))
    S(T,Evaluation(filteredMap))
  }
  

}
