package lars.tms.incr

/**
 * Created by hb on 7/16/15.
 */
sealed trait Result

object Result {
  case object success extends Result
  case object fail extends Result
}
