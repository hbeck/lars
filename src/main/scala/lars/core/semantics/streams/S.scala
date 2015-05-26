package lars.core.semantics.streams

/**
 * Created by hb on 5/26/15.
 */
case class S(T: Timeline, v: Evaluation) {

  def <= (other:S) = {
    (this.T <= other.T) && (this.v <= other.v)
  }

  def size = v.size

  def | (T1: Timeline) = S(T1,v|T1)

  def == (other: S) : Boolean = {
    this.T == other.T && this.v == other.v
  }

  def != (other: S) : Boolean = { !(this == other) }

  def ++ (other: S) = S (T ++ other.T, v ++ other.v)

  override def equals(that:Any) : Boolean = {
    that match {
      case other: S => this == other
      case _ => false
    }
  }

}


