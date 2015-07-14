package lars.aaai15

import lars.core.semantics.formulas.Term.str2Term
import lars.core.semantics.formulas.{Term, _}
import lars.core.semantics.streams.{Evaluation, S, Timeline}
import lars.core.semantics.structure.M
import lars.core.windowfn.time.{TimeWindow, TimeWindowParameters}
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
  case class jam(lineId: Term) extends Atom
  case class gc(vehicleId1: Term, vehicleId2:Term, lineId: Term) extends Atom
  case class old(vehicleId: Term) extends Atom

  val T = Timeline(0,50)
  val v = Evaluation(Map(36 -> Set(tram("a1","b")), 40 -> Set(tram("a3","h"))))
  val d = S(T,v)
  val SStar = d ++ S(T,Evaluation(Map(43 -> Set(exp("a3","m")), 44 -> Set(exp("a1","m")))))

   test("ex3") {
     val s1 = TimeWindow(d,42,TimeWindowParameters(4,0,1))
     def w = TimeWindow //alias
     val s2 = w(d,42,TimeWindowParameters(4,0,1))
     val s3 = w(d,42,4,0,1)
     val s4 = w(d,42,4)
     assert(s1 == s2)
     assert(s1 == s3)
     assert(s1 == s4)
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
    val tw = TimeWindow //alias
    val p = TimeWindowParameters //alias
    val a3 = C("a3")
    val m = C("m")
    //
    val wp5 = tw.toOp(0,5,1)
    val fm = WopFm(wp5,Diam(exp(a3,m)))
    //
    assert(mm/ss/42 ||- fm)
    //
    val s1 = tw(ss, 42, (0, 5, 1))
    assert(s1.T == (Timeline(42, 47)))
    assert(s1.v == vInt)
    assert(mm/s1/43 ||- exp(a3,m))
    //
    assert(mm/ss/43 ||- exp(a3,m))
  }

  test("ex6") {

    val w3 = TimeWindow.toOp(3)
    val wp5 = TimeWindow.toOp(0,5,1)
    val w20 = TimeWindow.toOp(20)
    val wp = TimeWindow.toOp(1); //TODO partition-based window
    //TODO ground vs non-ground -- do ground version first
//    val r2 = Rule(At("T",exp("Id","Y")), //TODO At with param
//                  wp(At("T1",tram("Id","X"))) and line("Id","L") and
//                  Not(w20(Diam(jam("X")))) and plan("L","X","Y","Z")
//                  and "T = T1 + Z") //TODO calculation
//    val r3 = Rule(gc("Id1","Id2","X"),
//                  At("T",exp("Id1","X")) and At("T",wp5(Diam(exp("Id2","X")))) and
//                  "Id1 != Id2" and Not(old("Id2")))

  }

}
