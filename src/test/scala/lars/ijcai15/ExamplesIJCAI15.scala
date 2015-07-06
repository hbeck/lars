package lars.ijcai15

import lars.core.Util
import lars.core.semantics.formulas._
import lars.core.semantics.programs.{Program, Rule}
import lars.core.semantics.streams.{Evaluation, S, Timeline}
import lars.core.windowfn.timebased.{TimeBasedWindow, TimeBasedWindowParam, TimeBasedWindowParameters}
import lars.strat.StratUtil
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
  def w = TimeBasedWindowParam
  val w3 = w.toOp2(m(3))
  val w5 = w.toOp2(m(5))
  val w1 = w.toOp2(m(1))
  val wp5 = w.toOp2(m(0),m(5),1)
  val M = D.toStructure()

  test("ex3") {
    val t = m(39.7)
    assert(t == 23820)
    val Sp = w(D,t,m(3))
    assert(Sp.T == Timeline(m(36.7),m(39.7)))
    assert(Sp.size == 2)
  }

  test("ex4") {
    val f = W(w3,(At(m(37.2),busG)))
    for (t <- m(37.2) to m(40.2)) {
      assert(M/t |= f)
    }
    assert(false == (M/(m(37.2)-1) |= f))
    assert(false == (M/(m(40.2)+1) |= f))
  }

  //for r1, r2, only relevant ground instances given
  val r1g = Rule(At(m(37.2)+m(3),expBusM), W(w3,At(m(37.2),busG)) and on)
  val r2g = Rule(At(m(39.1)+m(5),expTrM), W(w5,At(m(39.1),tramB) and on))
  val r3 = Rule(on, W(w1,Diam(request)))
  val r4 = Rule(takeBusM, W(wp5,Diam(expBusM)) and Not(takeTrM) and Not(W(w3,Diam(jam))))
  val r5 = Rule(takeTrM, W(wp5,Diam(expTrM)) and Not(takeBusM))
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
    assert((m1/t |= r1g.body))
    assert((m1/t |= r2g.body))
    assert((m1/t |= r3.body))
    assert((m1/t |= r4.body) == false)
    //
    assert((m1/m(44.1) |= expTrM))
    assert((m1/m(44.1) |= Diam(expTrM)))
    assert((m1/m(0) |= Diam(expTrM)))
    assert((m1/m(50) |= Diam(expTrM)))
    assert((m1/m(44.1) |= W(wp5,expTrM)))
    assert((m1/m(44.1) |= W(wp5,Diam(expTrM))))
    assert((m1/(m(44.1)+1) |= W(wp5,Diam(expTrM))) == false)
    assert((m1/(m(44.1)-1) |= W(wp5,Diam(expTrM))))
    assert((m1/(m(39.1)) |= W(wp5,Diam(expTrM))))
    assert((m1/(m(39.1)-1) |= W(wp5,Diam(expTrM))) == false)
    assert((m1/t |= takeBusM) == false)
    assert((m1/t |= Not(takeBusM)))
    //
    assert((m1/t |= r5.body))
    //
    val reductRules = Set[Rule](r1g,r2g,r3,r5);
    assert(P.rules.filter(m1/t |= _.body) == reductRules) //note: .sameElements also checks order
    //
    val PR1 = P.reduct(m1,t)
    assert(PR1.rules == reductRules)
    //manual model check for all rules of the reduct
    //r1g: At(m(37.2)+m(3),expBusM), w3(At(m(37.2),busG)) and on
    assert(m1/t |= W(w3,At(m(37.2),busG)))
    assert(m1/t |= on)
    assert(m1/t |= At(m(37.2)+m(3),expBusM))
    assert(m1/t |= Implies(And(W(w3,At(m(37.2),busG)),on),At(m(37.2)+m(3),expBusM)))
    //
    assert(m1/t |= r1g)
    assert(m1/t |= r2g)
    assert(m1/t |= r3)
    assert(m1/t |= r5)
    //
    assert(m1.isMinimalModel(PR1,t,Dp))
    //
    //
    assert(I1.isAnswerStream(P,Dp,t))
    assert(I2.isAnswerStream(P,Dp,t))
    //
    var X = Dp ++ S(T,Evaluation(common))
    assert(X.isAnswerStream(P,Dp,t) == false) //t -> takeTrM (or t -> takeBus) missing
    X = I1 + (t -> expTrM)
    assert(X.isAnswerStream(P,Dp,t) == false) //non minimal
    X = I1 - (m(44.1) -> expTrM)
    assert(X.isAnswerStream(P,Dp,t) == false) //m(44.1) -> expTrM missing
    //
    //
    var cntAll=0
    var cntModels=0
    val A = (I1 ++ I2) -- Dp
    for (addS <- A.properSubstreams()) {
      cntAll += 1
      val candidateS = Dp ++ addS
      if (candidateS.isAnswerStream(P,Dp,t)) {
        assert(candidateS == I1 || candidateS == I2)
          cntModels += 1
      } else if (candidateS.toStructure(Set()).isModel(P,t)) {
          cntModels += 1
      }
    }
    println("checked "+cntAll+" interpretations")
    println(""+cntModels+" models")
    //using all atoms yields a model
    assert((Dp++A).toStructure(Set()).isModel(P,t))
  }

  test("ex7") {
    val t:Int = 0
    object x extends Atom
    object y extends Atom
    val w3fn = TimeBasedWindow(TimeBasedWindowParameters(3,0,1))
    val w3 = WindowOperator2(w3fn)
    //TODO At(t,x) vs AtAtom(t,x)
    val Pp = Program(Set(Rule(AtAtom(t,x),WAtAtom(w3,AtAtom(t,y)))))
    //
    val returnedEAtoms: Set[ExtendedAtom] = StratUtil.extendedAtoms(Pp)
    val expectedEAtoms: Set[ExtendedAtom] = Set(AtAtom(t,x),x,WAtAtom(w3,AtAtom(t,y)),AtAtom(t,y),y)
    assert(returnedEAtoms == expectedEAtoms)
    //TODO
  }

}
