package lars.core

import scala.language.implicitConversions
import lars.core.Formulas.Atom

/**
 * Created by hb on 1/2/15.
 */

object LStreams {

  case class TimePoint(timePoint: Int) {
    def in (timeline:Timeline) : Boolean = {
      timePoint >= timeline.lower && timePoint <= timeline.upper
    }
  }

  case class Timeline(lower: Int, upper: Int) {
    assert(lower <= upper)
    def timePoints = lower until (upper + 1)
    def <= (other: Timeline) = {
      this.lower >= other.lower && this.upper <= other.upper
    }
    override def toString = "["+lower+","+upper+"]"
  }

  //TODO function instead?
  case class Evaluation(mapping:Map[Int,Set[Atom]]) {
    //restriction
    def | (T: Timeline) = Evaluation(mapping filterKeys { T.timePoints contains _ })

    def at(t:Int) = {
      if (mapping.contains(t))
        mapping.apply(t)
      else
        Set[Atom]()
      //mapping.applyOrElse(t, Set[Atom]())
    }

    def <= (other: Evaluation) = {
      def subseteqAt(t:Int) = {
        at(t) subsetOf (other at(t))
      }
      mapping.keys.forall { subseteqAt }
    }

    override def toString = {
      val sb = new StringBuilder()
      sb.append("{")
      for ((k, v) <- mapping) {
        sb.append(" " + k + " -> " + v)
      }
      sb.append(" }")
      sb.toString()
    }
  }

  case class LStream(T: Timeline, v: Evaluation) {
    def <= (other:LStream) = {
      (this.T <= other.T) && (this.v <= other.v)
    }
  }

  implicit def Int2TimePoint(timePoint:Int) = TimePoint(timePoint)
  implicit def Pair2Timeline(tuple: (Int,Int)) = Timeline(tuple._1,tuple._2)
  implicit def Map2Evaluation(map:Map[Int,Set[Atom]]) = Evaluation(map)
}
