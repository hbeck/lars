package lars.core

import lars.core.LStreams.LStream

/**
 * Created by hb on 5/4/15.
 */
object WindowOperators {

  abstract class StreamChoice() extends ((LStream, LStream) => LStream)

  val ch1 = new StreamChoice() {
    override def apply(s1: LStream, s2: LStream) : LStream = s1
  }

  val ch2 = new StreamChoice() {
    override def apply(s1: LStream, s2: LStream) : LStream = s2
  }

}
