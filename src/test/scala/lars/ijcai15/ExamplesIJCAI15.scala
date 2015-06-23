package lars.ijcai15

import lars.core.semantics.formulas._
import lars.core.semantics.streams.{Evaluation, S, Timeline}
import lars.core.windowfn.timebased.TimeBasedWindow
import org.scalatest.FunSuite

/**
 * Created by hb on 5/19/15.
 */
class ExamplesIJCAI15 extends FunSuite {

  object busG extends Atom
  object tramB extends Atom
  object expBusM extends Atom
  object expTrM extends Atom
  object on extends Atom
  object request extends Atom
  object takeBusM extends Atom
  object takeTrM extends Atom
  object jam extends Atom

  def m(i:Double) = i*10*60 toInt

  val T = Timeline(0,m(50))
  val v = Evaluation(Map(m(37.2) -> Set(busG), m(39.1) -> Set(tramB)))
  val D = S(T,v)

  test("ex3") {
    def w = TimeBasedWindow
    val t = m(39.7)
    assert(t == 23820)
    val Sp = w(D,t,m(3))
    assert(Sp.T == Timeline(m(36.7),m(39.7)))
    assert(Sp.size == 2)
  }

//  test("ex4") {
//    val B = Set[Atom](plan("l1","b","m","8"),
//      plan("l2","g","m","7"),
//      plan("l3","h","m","3"),
//      line("a1","l1"),
//      line("a2","l2"),
//      line("a3","l3"))
//    val T = Timeline(0,50)
//    val v = Evaluation(Map(36 -> Set(tram("a1","b")), 40 -> Set(tram("a3","h"))))
//    val d = S(T,v)
//    val vInt = Evaluation(Map(43 -> Set(exp("a3","m")), 44 -> Set(exp("a1","m"))))
//    val ss = d ++ S(T,vInt)
//    val mm = M(ss.T,ss.v,B) //bad
//    val tw = TimeBasedWindow //alias
//    val p = TimeBasedWindowParameters //alias
//    val a3 = C("a3")
//    val m = C("m")
//    //
//    val fm1 = W(tw,ch2,p(0,5,1),D(exp(a3,m)))
//    val fm2 = fm1 // TODO W(tw,ch2,(0,5,1),D(a)); TODO W2(tw,(0,5,1),D(a))
//    //
//    for (f <- List(fm1,fm2)) {
//      assert(mm/ss/42 ||- fm1)
//      //
//      val s1 = tw(ss, 42, (0, 5, 1))
//      assert(s1.T == (Timeline(42, 47)))
//      assert(s1.v == vInt)
//      assert(mm/s1/43 ||- exp(a3,m))
//      //
//      assert(mm/ss/43 ||- exp(a3,m))
//    }
//
//  }

}
