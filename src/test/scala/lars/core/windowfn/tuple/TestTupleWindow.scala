package lars.core.windowfn.tuple

import lars.core.semantics.streams.{S, Timeline}
import org.scalatest.FunSuite

import collection.immutable.HashMap

/**
 * Created by hb on 7/14/15.
 */
class TestTupleWindow extends FunSuite {

  test("test 1") {
    val T = Timeline(0,5)
    val s = new HashMap[Int,S](
      0 -> S(T)
    )
  }

}
