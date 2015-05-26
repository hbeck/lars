package lars.core.semantics.formulas

import lars.core.semantics.streams.S

/**
 * Created by hb on 5/4/15.
 */
object WindowOperators {

  abstract class StreamChoice() extends ((S, S) => S)

  object ch1 extends StreamChoice() {
    override def apply(s1: S, s2: S) : S = s1
  }

  object ch2 extends StreamChoice() {
    override def apply(s1: S, s2: S) : S = s2
  }

}
