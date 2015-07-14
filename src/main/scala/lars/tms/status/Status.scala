package lars.tms.status

/**
 * Created by hb on 7/14/15.
 */
sealed trait Status

object Status {
  case object in extends Status
  case object out extends Status
  case object unknown extends Status
}
