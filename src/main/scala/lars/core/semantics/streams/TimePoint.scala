package lars.core.semantics.streams

/**
 * Created by hb on 5/26/15.
 */
case class TimePoint(timePoint: Int) {
  def in (timeline:Timeline) : Boolean = {
    timePoint >= timeline.lower && timePoint <= timeline.upper
  }
}