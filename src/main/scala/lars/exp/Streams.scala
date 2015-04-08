package lars.exp

import lars.exp.Formulas.Atom

/**
 * Created by hb on 1/2/15.
 */

object Streams {

  case class TimePoint(timePoint: Int) {
    def in (timeline:Timeline) : Boolean = {
      timePoint >= timeline.lower && timePoint <= timeline.upper
    }
  }

  case class Timeline(lower: Int, upper: Int) {
    def timePoints = lower until (upper + 1)
    override def toString = "["+lower+","+upper+"]"
  }

  case class Evaluation(map:Map[Int,Set[Atom]]) {

    //restriction
    def | (T:Timeline) = Evaluation(map.filterKeys{ t => T.timePoints contains t })

    override def toString = {
      val sb = new StringBuilder()
      sb.append("{")
      for ((k, v) <- map) {
        sb.append(" " + k + " -> " + v)
      }
      sb.append(" }")
      sb.toString()
    }
  }

  case class Stream(T: Timeline, v: Evaluation)

  implicit def Int2TimePoint(timePoint:Int) = TimePoint(timePoint)

}
