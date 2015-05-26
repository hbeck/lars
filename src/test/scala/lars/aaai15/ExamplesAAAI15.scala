package lars.aaai15

import lars.core.semantics.formulas._
import WindowOperators.ch2
import lars.core.semantics.streams.{S, Evaluation, Timeline}
import lars.core.semantics.structure.M
import lars.core.windowfn.timebased.{TimeBasedWindowParameters, TimeBasedWindow}
import org.scalatest.FunSuite

/**
 * Created by hb on 5/19/15.
 */
class ExamplesAAAI15 extends FunSuite {

  val T = Timeline(0,50)
  val v = Evaluation(Map(36 -> Set(Atom("tram","a1","b")), 40 -> Set(Atom("tram","a3","h"))))
  val d = S(T,v)
  val SStar = d ++ S(T,Evaluation(Map(43 -> Set(Atom("exp","a3","m")), 44 -> Set(Atom("exp","a1","m")))))

   test("ex3") {
     val s1 = TimeBasedWindow(d,42,(4,0,1))
     val s2 = TimeBasedWindow(d,42,4,0,1)
     val s3 = TimeBasedWindow(d,42,4)
     def w = TimeBasedWindow
     val S4 = w(d,42,4)
     assert(s1 == s2)
     assert(s1 == s3)
     assert(s1 == S4)
     val v1 = Evaluation(Map(40 -> Set(Atom("tram","a3","h"))))
     assert(s1 == S((38,42),v1))
   }

  test("ex4") {
    val B = Set(Atom("plan","l1","b","m","8"),
      Atom("plan","l2","g","m","7"),
      Atom("plan","l3","h","m","3"),
      Atom("line","a1,l1"),
      Atom("line","a2","l2"),
      Atom("line","a3","l3"))
    val T = Timeline(0,50)
    val v = Evaluation(Map(36 -> Set(Atom("tram","a1","b")), 40 -> Set(Atom("tram","a3","h"))))
    val d = S(T,v)
    val vInt = Evaluation(Map(43 -> Set(Atom("exp","a3","m")), 44 -> Set(Atom("exp","a1","m"))))
    val ss = d ++ S(T,vInt)
    val m = M(ss.T,ss.v,B)
    val a = Atom("exp","a3","m")
    val tw = TimeBasedWindow //alias
    //
    val fm1 = W(tw,ch2,TimeBasedWindowParameters(0,5,1),D(a))
    val fm2 = fm1 // TODO W(tw,ch2,(0,5,1),D(a)); TODO W2(tw,(0,5,1),D(a))
    //
    for (f <- List(fm1,fm2)) {
      assert(m / ss / 42 ||- fm1)
      //
      val s1 = tw(ss, 42, (0, 5, 1))
      assert(s1.T == (Timeline(42, 47)))
      assert(s1.v == vInt)
      assert(m / s1 / 43 ||- a)
      //
      assert(m / ss / 43 ||- a)
    }

  }

}
