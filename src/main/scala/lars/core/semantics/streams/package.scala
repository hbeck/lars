package lars.core.semantics

import lars.core.semantics.formulas.Atom

/**
 * Created by hb on 5/26/15.
 */
package object streams {

  implicit def Map2Evaluation(map:Map[Int,Set[Atom]]) = Evaluation(map)
  implicit def Int2TimePoint(timePoint:Int) = TimePoint(timePoint)
  implicit def Pair2Timeline(tuple: (Int,Int)) = Timeline(tuple._1,tuple._2)

}
