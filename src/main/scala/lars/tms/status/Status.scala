package lars.tms.status

/**
 * Created by hb on 7/14/15.
 */
abstract class Status

case class in() extends Status
case class out() extends Status
case class unknown() extends Status
