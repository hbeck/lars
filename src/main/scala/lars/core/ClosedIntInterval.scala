package lars.core

/**
 * Created by hb on 7/14/15.
 */
class ClosedIntInterval(lower:Int, upper:Int) extends Interval[Int](lower,upper) {

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

  def ++ [T <: ClosedIntInterval](other: T) = new ClosedIntInterval(Math.min(lower,other.lower),Math.max(upper,other.upper)).asInstanceOf[T]

  def + [T <: ClosedIntInterval](i: Int) : T = {
    if (this contains i)
      this.asInstanceOf[T]
    else if (i < lower)
      new ClosedIntInterval(i,upper).asInstanceOf[T]
    else
      new ClosedIntInterval(lower,i).asInstanceOf[T]
  }

}
