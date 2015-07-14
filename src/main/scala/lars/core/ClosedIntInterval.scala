package lars.core

/**
 * Created by hb on 7/14/15.
 */
case class ClosedIntInterval(lower:Int, upper:Int) extends Interval[Int](lower,upper) {

  assert(lower <= upper)

  def toSeq = lower until (upper + 1)

  def <= (other: ClosedIntInterval) = {
    this.lower >= other.lower && this.upper <= other.upper
  }

  override def equals(that:Any) = {
    that match {
      case other: ClosedIntInterval => this == other
      case _ => false
    }
  }

  override def contains(i: Int): Boolean = lower <= i && i <= upper

  def ++ (other: ClosedIntInterval) = ClosedIntInterval(Math.min(lower,other.lower),Math.max(upper,other.upper))

  def + (i: Int) : ClosedIntInterval = {
    if (this contains i)
      this
    else if (i < lower)
      ClosedIntInterval(i,upper)
    else
      ClosedIntInterval(lower,i)
  }

}
