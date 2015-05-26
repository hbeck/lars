package lars.core.semantics.streams

/**
 * Created by hb on 5/26/15.
 */
case class Timeline(lower: Int, upper: Int) {

  assert(lower <= upper)

  //TODO remove timePoints and let Timeline extend TraversableOnce etc
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