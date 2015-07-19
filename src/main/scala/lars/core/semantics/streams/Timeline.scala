package lars.core.semantics.streams

import lars.core.ClosedIntInterval

/**
 * Created by hb on 5/26/15.
 */
case class Timeline(override val lower: Int, override val upper: Int) extends ClosedIntInterval(lower,upper) {

  override def equals(that:Any) = {
    that match {
      case other: Timeline => this == other
      case _ => false
    }
  }

  def ++(other:Timeline): Timeline = {
    val c:ClosedIntInterval = super.++(other)
    Timeline(c)
  }

  def +(i: Int): Timeline = {
    val c:ClosedIntInterval = super.+(i)
    Timeline(c)
  }

  //TODO remove timePoints and let Timeline extend TraversableOnce etc
  def timePoints = toSeq
}

object Timeline {
  def apply(c:ClosedIntInterval): Timeline = Timeline(c.lower,c.upper)
}