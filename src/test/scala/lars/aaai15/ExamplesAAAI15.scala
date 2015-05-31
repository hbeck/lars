package lars.aaai15

import lars.core.semantics.formulas._
import lars.core.semantics.formulas.Term
import WindowOperators.ch2
import lars.core.semantics.streams.{S, Evaluation, Timeline}
import lars.core.semantics.structure.M
import lars.core.Util.str2Term
import lars.core.windowfn.timebased.{TimeBasedWindowParameters, TimeBasedWindow}
import org.scalatest.FunSuite

/**
 * Created by hb on 5/19/15.
 */
class ExamplesAAAI15 extends FunSuite {

  //case class Atom1(t:Term) extends Atom
  //case class Atom2(t1:Term, t2: Term) extends Atom
  //case class Atom3(t1:Term, t2: Term, t3: Term) extends Atom
  //case class Atom4(t1:Term, t2: Term, t3: Term, t4: Atom) extends Atom

  //better: tram(vehicleId,station) extends Atom2
  case class tram(vehicleId:Term, station: Term) extends Atom
  case class exp(vehicleId:Term, station: Term) extends Atom
  case class plan(lineId:Term, fromStation: Term, toStation:Term, dur: Term) extends Atom
  case class line(vehicleId: Term, lineId:Term) extends Atom

  val T = Timeline(0,50)
  val v = Evaluation(Map(36 -> Set(tram("a1","b")), 40 -> Set(tram("a3","h"))))
  val d = S(T,v)
  val SStar = d ++ S(T,Evaluation(Map(43 -> Set(exp("a3","m")), 44 -> Set(exp("a1","m")))))

   test("ex3") {
     val s1 = TimeBasedWindow(d,42,(4,0,1))
     val s2 = TimeBasedWindow(d,42,4,0,1)
     val s3 = TimeBasedWindow(d,42,4)
     def w = TimeBasedWindow
     val S4 = w(d,42,4)
     assert(s1 == s2)
     assert(s1 == s3)
     assert(s1 == S4)
     val v1 = Evaluation(Map(40 -> Set(tram("a3","h"))))
     assert(s1 == S((38,42),v1))
   }

  test("ex4") {
    val B = Set[Atom](plan("l1","b","m","8"),
      plan("l2","g","m","7"),
      plan("l3","h","m","3"),
      line("a1","l1"),
      line("a2","l2"),
      line("a3","l3"))
    val T = Timeline(0,50)
    val v = Evaluation(Map(36 -> Set(tram("a1","b")), 40 -> Set(tram("a3","h"))))
    val d = S(T,v)
    val vInt = Evaluation(Map(43 -> Set(exp("a3","m")), 44 -> Set(exp("a1","m"))))
    val ss = d ++ S(T,vInt)
    val mm = M(ss.T,ss.v,B) //bad
    val tw = TimeBasedWindow //alias
    val p = TimeBasedWindowParameters //alias
    val a3 = C("a3")
    val m = C("m")
    //
    val fm1 = W(tw,ch2,p(0,5,1),D(exp(a3,m)))
    val fm2 = fm1 // TODO W(tw,ch2,(0,5,1),D(a)); TODO W2(tw,(0,5,1),D(a))
    //
    for (f <- List(fm1,fm2)) {
      assert(mm/ss/42 ||- fm1)
      //
      val s1 = tw(ss, 42, (0, 5, 1))
      assert(s1.T == (Timeline(42, 47)))
      assert(s1.v == vInt)
      assert(mm/s1/43 ||- exp(a3,m))
      //
      assert(mm/ss/43 ||- exp(a3,m))
    }

  }

}
