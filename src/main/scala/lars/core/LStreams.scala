package lars.core

import scala.language.implicitConversions
import lars.core.Formulas.Atom
import lars.core.Util.merge

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
    def == (other: Timeline) = this.lower == other.lower && this.upper == other.upper
    override def equals(that:Any) = {
      that match {
        case other: Timeline => this == other
        case _ => false
      }
    }
    def != (other: Timeline) : Boolean = { !(this == other) }
    def ++ (other: Timeline) = Timeline(Math.min(lower,other.lower),Math.max(upper,other.upper))
    def + (t: Int) : Timeline = {
      if (this contains t)
        this
      else if (t < lower)
        Timeline(t,upper)
      else
        Timeline(lower,t)
    }
    def contains(t: Int) = lower <= t && t <= upper
    override def toString = "["+lower+","+upper+"]"
  }

  case class Evaluation(mapping:Map[Int,Set[Atom]]) extends (Int => Set[Atom]) {

    def apply(t:Int) = mapping.getOrElse(t,Set[Atom]())

    //restriction
    def | (T: Timeline) = Evaluation(mapping filterKeys { T contains _ })

    def <= (other: Evaluation) = {
      def subseteqAt(t:Int) = {
        apply(t) subsetOf (other (t)) //TODO
      }
      mapping.keys.forall { subseteqAt }
    }

    def == (other: Evaluation) : Boolean = {
      if (this.mapping.size != other.mapping.size) return false
      for (k <- mapping.keys) {
        val m0 = this.mapping.apply(k)
        val m1 = other.mapping.apply(k)
        if (!m0.equals(m1)) return false
      }
      true
    }
    def != (other: Evaluation) : Boolean = { !(this == other) }

    def ++ (other:Evaluation) = Evaluation(merge(mapping,other.mapping))

    override def equals(that:Any) = {
      that match {
        case other: Evaluation => this == other
        case _ => false
      }
    }

    def size : Int = Math.max(1,mapping.values.flatten.size)

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
    def size = v.size
    def | (T1: Timeline) = LStream(T1,v|T1)
    def == (other: LStream) : Boolean = {
      this.T == other.T && this.v == other.v
    }
    def != (other: LStream) : Boolean = { !(this == other) }
    def ++ (other: LStream) = LStream (T ++ other.T, v ++ other.v)
    override def equals(that:Any) : Boolean = {
      that match {
        case other: LStream => this == other
        case _ => false
      }
    }
  }

  implicit def Int2TimePoint(timePoint:Int) = TimePoint(timePoint)
  implicit def Pair2Timeline(tuple: (Int,Int)) = Timeline(tuple._1,tuple._2)
  implicit def Map2Evaluation(map:Map[Int,Set[Atom]]) = Evaluation(map)
}
