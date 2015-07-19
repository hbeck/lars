package lars.strat

/**
 * Created by hb on 7/6/15.
 */
sealed abstract class Dependency

case object eql extends Dependency {
  override def toString = "="
}
case object grt extends Dependency {
  override def toString = ">"
}
case object geq extends Dependency {
  override def toString = ">="
}
