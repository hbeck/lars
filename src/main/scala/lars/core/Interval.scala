package lars.core

/**
 * Created by hb on 7/14/15.
 */
abstract class Interval[T](val lower:T, val upper:T) {

  def == (other: Interval[T]) = this.lower == other.lower && this.upper == other.upper

  def != (other: Interval[T]) : Boolean = { !(this == other) }

  def contains(x: T): Boolean

  override def toString = "["+lower+","+upper+"]"

}
