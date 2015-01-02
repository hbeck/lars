package lars.exp

import lars.exp.Formulas.Atom

/**
 * Created by hb on 1/2/15.
 */

object Streams {

  case class Timepoint(timepoint: Int) {
    def in (timeline:Timeline) : Boolean = {
      timepoint >= timeline.lower && timepoint <= timeline.upper
    }
  }

  case class Timeline(lower: Int, upper: Int) {
    def timepoints = lower until (upper + 1)
    override def toString = "["+lower+","+upper+"]"
  }

  case class Evaluation(map:Map[Int,Set[Atom]]) {
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

  implicit def Int2Timepoint(timepoint:Int) = Timepoint(timepoint)

}
