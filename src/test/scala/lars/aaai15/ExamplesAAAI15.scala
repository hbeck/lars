package lars.aaai15

import lars.core.Formulas.Atom
import lars.core.LStreams.{Evaluation, LStream, Timeline}
import lars.core.windowfn.TimeBasedWindow
import org.scalatest.FunSuite

/**
 * Created by hb on 5/19/15.
 */
class ExamplesAAAI15 extends FunSuite {

  val T = Timeline(0,50)
  val v = Evaluation(Map(36 -> Set(Atom("tram","a1","b")), 40 -> Set(Atom("tram","a3","h"))))
  val D = LStream(T,v)
  val SStar = D ++ LStream(T,Evaluation(Map(43 -> Set(Atom("exp","a3","m")), 44 -> Set(Atom("exp","a1","m")))))

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
