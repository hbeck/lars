package lars.ijcai15

import lars.core.Util
import lars.core.semantics.formulas._
import lars.core.semantics.programs.{Program, Rule}
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

  def m(i:Double) = (i*10*60).toInt

  val T = Timeline(0,m(50))
  val v = Evaluation(Map(m(37.2) -> Set(busG), m(39.1) -> Set(tramB)))
  val D = S(T,v)
  //
  def w = TimeBasedWindow
  val w3 = w.toOp(m(3))
  val w5 = w.toOp(m(5))
  val w1 = w.toOp(m(1))
  val wp5 = w.toOp((m(0),m(1),1))
  val M = D.toStructure()

  test("ex3") {
    val t = m(39.7)
    assert(t == 23820)
    val Sp = w(D,t,m(3))
    assert(Sp.T == Timeline(m(36.7),m(39.7)))
    assert(Sp.size == 2)
  }

  test("ex4") {
    val f = w3(At(m(37.2),busG))
    for (t <- m(37.2) to m(40.2)) {
      assert(M/t |= f)
    }
    assert(false == (M/(m(37.2)-1) |= f))
    assert(false == (M/(m(40.2)+1) |= f))
  }

  //for r1, r2, only relevant ground instances given
  val r1g = Rule(At(m(37.2)+m(3),expBusM), w3(At(m(37.2),busG)) and on)
  val r2g = Rule(At(m(39.1)+m(5),expTrM), w5(At(m(39.1),tramB) and on))
  val r3 = Rule(on, w1(Diam(request)))
  val r4 = Rule(takeBusM, wp5(Diam(expBusM)) and Not(takeTrM) and Not(w3(Diam(jam))))
  val r5 = Rule(takeTrM, wp5(Diam(expTrM)) and Not(takeBusM))
  //
  val P = Program(Set(r1g,r2g,r3,r4,r5))

  test("ex6") {
    val t = m(39.7)
    val Dp = D + (t -> request)
    val common = Map[Int,Set[Atom]](m(40.2) -> Set(expBusM), m(44.1) -> Set(expTrM), t -> Set(on))
    val mI1 = Util.merge(Map[Int,Set[Atom]](t -> Set(takeTrM)), common)
    val mI2 = Util.merge(Map[Int,Set[Atom]](t -> Set(takeBusM)), common)
    //
    val I1 = Dp ++ S(T,Evaluation(mI1))
    val I2 = Dp ++ S(T,Evaluation(mI2))
    //
    // steps for i1
    val m1 = I1.toStructure(Set[Atom]())
    val mt1 = m1/t
    assert((mt1 |= r1g.body))
    assert((mt1 |= r2g.body))
    assert((mt1 |= r3.body))
    assert((mt1 |= r4.body) == false)
    //
    assert((m1/m(44.1) |= expTrM))
    //TODO whole formula bottom up
    assert((mt1 |= r5.body))
    assert(P.rules.filter(mt1 |= _.body).sameElements(Set[Rule](r1g,r2g,r3,r5)))
    val R1 = P.reduct(m1,t)
    //
    m1.isMinimalModel(R1,t,Dp)


    //
    //
    assert(I1.isAnswerStream(P,Dp,t))
    assert(I2.isAnswerStream(P,Dp,t))
    // TODO assert no other answer stream
  }

}
