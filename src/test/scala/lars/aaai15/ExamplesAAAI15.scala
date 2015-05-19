package lars.aaai15

import lars.core.Formulas.Atom
import lars.core.LStreams.{Evaluation, LStream, Timeline}
import lars.core.windowfn.TimeBasedWindow
import org.scalatest.FunSuite

/**
 * Created by hb on 5/19/15.
 */
class ExamplesAAAI15 extends FunSuite {

  val plan = Set(("l1","b","m",8),("l2","g","m",7),("l3","h","m",3))
  val line = Set(("a1,l1"),("a2","l2"),("a3","l3"))
  val T = Timeline(0,50)
  val v = Evaluation(Map(36 -> Set(Atom("tram","a1","b")), 40 -> Set(Atom("tram","a3","h"))))
  val D = LStream(T,v)

   test("ex3") {
     val S1 = TimeBasedWindow(D,42,(4,0,1))
     val S2 = TimeBasedWindow(D,42,4,0,1)
     val S3 = TimeBasedWindow(D,42,4)
     def w = TimeBasedWindow
     val S4 = w(D,42,4)
     assert(S1 == S2)
     assert(S1 == S3)
     assert(S1 == S4)
     val v1 = Evaluation(Map(40 -> Set(Atom("tram","a3","h"))))
     assert(S1 == LStream((38,42),v1))
   }

}
