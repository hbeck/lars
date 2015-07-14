package lars.ijcai15

import lars.core.MapUtils
import lars.core.semantics.formulas._
import lars.core.semantics.programs.{AS, Program, Rule}
import lars.core.semantics.streams.{Evaluation, S, Timeline}
import lars.core.windowfn.time.{TimeWindow, TimeWindowFixedParams, TimeWindowParameters}
import lars.strat._
import lars.strat.alg.Stratify
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
  def w = TimeWindow
  val wop3 = w.toOp(m(3))
  val wop5 = w.toOp(m(5))
  val wop1 = w.toOp(m(1))
  val wopP5 = w.toOp(m(0),m(5),1)
  val M = D.toStructure()

  test("ex3") {
    val t = m(39.7)
    assert(t == 23820)
    val Sp = w(D,t,m(3))
    assert(Sp.T == Timeline(m(36.7),m(39.7)))
    assert(Sp.size == 2)
  }

  test("ex4") {
    val sfm = At(m(37.2), busG)
    def x = TimeWindowParameters
    val fm1 = W(w, x(m(3)), sfm)
    val fm2 = Wop(wop3, sfm) //alt
    for (t <- m(37.2) to m(40.2)) {
      for (fm <- Seq(fm1, fm2))
        assert(M / t |= fm)
    }
    for (fm <- Seq(fm1, fm2)) {
      assert(false == (M / (m(37.2) - 1) |= fm))
      assert(false == (M / (m(40.2) + 1) |= fm))
    }
  }

  //for r1, r2, only relevant ground instances given
  val r1g = Rule(At(m(37.2)+m(3),expBusM), Wop(wop3,At(m(37.2),busG)) and on)
  val r2g = Rule(At(m(39.1)+m(5),expTrM), Wop(wop5,At(m(39.1),tramB)) and on)
  val r3 = Rule(on, Wop(wop1,Diam(request)))
  val r4 = Rule(takeBusM, Wop(wopP5,Diam(expBusM)) and Not(takeTrM) and Not(Wop(wop3,Diam(jam))))
  val r5 = Rule(takeTrM, Wop(wopP5,Diam(expTrM)) and Not(takeBusM))
//  for (rule <- Set (r1g,r2g,r3,r4,r5)) {
//    println(rule)
//  }
  //
  val P = Program(Set(r1g,r2g,r3,r4,r5))
  val t = m(39.7)
  val Dp = D + (t -> request)

  test("ex6") {
    val common = Map[Int,Set[Atom]](m(40.2) -> Set(expBusM), m(44.1) -> Set(expTrM), t -> Set(on))
    val mI1 = MapUtils.merge(Map[Int,Set[Atom]](t -> Set(takeTrM)), common)
    val mI2 = MapUtils.merge(Map[Int,Set[Atom]](t -> Set(takeBusM)), common)
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
    assert((m1/m(44.1) |= Wop(wopP5,expTrM)))
    assert((m1/m(44.1) |= Wop(wopP5,Diam(expTrM))))
    assert((m1/(m(44.1)+1) |= Wop(wopP5,Diam(expTrM))) == false)
    assert((m1/(m(44.1)-1) |= Wop(wopP5,Diam(expTrM))))
    assert((m1/(m(39.1)) |= Wop(wopP5,Diam(expTrM))))
    assert((m1/(m(39.1)-1) |= Wop(wopP5,Diam(expTrM))) == false)
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
    assert(m1/t |= Wop(wop3,At(m(37.2),busG)))
    assert(m1/t |= on)
    assert(m1/t |= At(m(37.2)+m(3),expBusM))
    assert(m1/t |= Implies(And(Wop(wop3,At(m(37.2),busG)),on),At(m(37.2)+m(3),expBusM)))
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
    //using all atoms yields a model
    assert((I1 ++ I2).toStructure(Set()).isModel(P,t))
    //
    val answerStreams = AS(P,Dp,t)
    assert(answerStreams.size == 2)
//    println("answer streams: "+answerStreams.size)
//    for (as <- answerStreams) {
//      println(as)
//    }
  }

  val w3fn = TimeWindowFixedParams(TimeWindowParameters(3,0,1))
  val w3 = WindowOperatorFixedParams(w3fn)
  object x extends Atom
  object y extends Atom
  val Pp = Program(Set(Rule(AtAtom(t,x),WAtAtom(w3,AtAtom(t,y)))))
  //TODO At(t,x) vs AtAtom(t,x)

  test("ex7") {
    val expectedEAtoms: Set[ExtendedAtom] = Set(AtAtom(t,x),x,WAtAtom(w3,AtAtom(t,y)),AtAtom(t,y),y)
    val actualEAtoms: Set[ExtendedAtom] = ExtendedAtoms(Pp,true)
    assert(actualEAtoms == expectedEAtoms)
    //
    def e(from:ExtendedAtom, to:ExtendedAtom, d:Dep) = DepEdge(from, to, d)
    val nodes = ExtendedAtoms(Pp,true)
    val expectedSDG = DepGraph(nodes,Set[DepEdge](
      e(AtAtom(t,x),WAtAtom(w3,AtAtom(t,y)),geq),
      e(WAtAtom(w3,AtAtom(t,y)),y,grt),
      e(AtAtom(t,x),x,eql),
      e(x,AtAtom(t,x),eql),
      e(AtAtom(t,y),y,eql),
      e(y,AtAtom(t,y),eql)
    ))
    val actualSDG = DepGraph(Pp)
//    for (e <- actualSDG.edges) {
//      println(e)
//    }
    for (e <- actualSDG.edges) {
      assert(expectedSDG.edges contains e)
    }
    for (e <- expectedSDG.edges) {
      assert(actualSDG.edges contains e)
    }
    assert(actualSDG.edges == expectedSDG.edges)
    assert(actualSDG == expectedSDG)
  }

  test("ex8") {
    val opt: Option[Stratification] = Stratify(Pp)
    assert(opt.isDefined)
    val strat = opt.get
    assert(strat.maxStratum == 2)
    assert(strat(0) == Set(AtAtom(t,y),y))
    assert(strat(1) == Set(WAtAtom(w3,AtAtom(t,y))))
    assert(strat(2) == Set(AtAtom(t,x),x))
    // can call also in direction as given in paper:
    assert(strat(AtAtom(t,y)) == 0)
    assert(strat(y) == 0)
    assert(strat(WAtAtom(w3,AtAtom(t,y))) == 1)
    assert(strat(AtAtom(t,x)) == 2)
    assert(strat(x) == 2)
    // decision was made to have a 'maximal' stratification in the algorithm
    // however, ex8 as presented in the paper is also a stratification:
    assert(
      Stratification.isStratification(Map(
      AtAtom(t,y) -> 0,
      y -> 0,
      WAtAtom(w3,AtAtom(t,y)) -> 1,
      AtAtom(t,x) -> 1,
      x -> 1),
      Pp))
    //
    assert(Strata(Pp)(0)==Pp)
  }

  /*
  val r1g = Rule(At(m(37.2)+m(3),expBusM), Wop(wop3,At(m(37.2),busG)) and on)
  val r2g = Rule(At(m(39.1)+m(5),expTrM), Wop(wop5,At(m(39.1),tramB)) and on)
  val r3 = Rule(on, Wop(wop1,Diam(request)))
  val r4 = Rule(takeBusM, Wop(wopP5,Diam(expBusM)) and Not(takeTrM) and Not(Wop(wop3,Diam(jam))))
  val r5 = Rule(takeTrM, Wop(wopP5,Diam(expTrM)) and Not(takeBusM))
  */
  
  test("ex9") {
    val extendedAtoms: Set[ExtendedAtom] = ExtendedAtoms(P,true)
    val expectedExtendedAtoms = Set(
      AtAtom(m(37.2)+m(3),expBusM), expBusM, WAtAtom(wop3,AtAtom(m(37.2),busG)), AtAtom(m(37.2),busG), busG, on,
      AtAtom(m(39.1)+m(5),expTrM), expTrM, WAtAtom(wop5,AtAtom(m(39.1),tramB)), AtAtom(m(39.1),tramB), tramB,
      WDiamAtom(wop1,DiamAtom(request)), request,
      takeBusM, WDiamAtom(wopP5,DiamAtom(expBusM)), takeTrM, WDiamAtom(wop3,DiamAtom(jam)), jam,
      WDiamAtom(wopP5,DiamAtom(expTrM)), expTrM
    )
    assert(extendedAtoms == expectedExtendedAtoms)
    //
    val strat = Stratify(P).get
    //
    assert(strat.maxStratum == 5)
    // 5
    for (x <- Set(takeBusM, takeTrM)) {
      assert(strat(x) == 5)
    }
    // 4
    for (x <- Set(WDiamAtom(wopP5,DiamAtom(expBusM)),WDiamAtom(wop3,DiamAtom(jam)),WDiamAtom(wopP5,DiamAtom(expTrM)))) {
      assert(strat(x) == 4)
    }
    // 3
    for (x <- Set(AtAtom(m(37.2)+m(3),expBusM), expBusM, AtAtom(m(39.1)+m(5),expTrM), expTrM, jam)) {
      assert(strat(x) == 3)
    }
    // 2
    for (x <- Set(on,WAtAtom(wop3,AtAtom(m(37.2),busG)),WAtAtom(wop5,AtAtom(m(39.1),tramB)))) {
      assert(strat(x) == 2)
    }
    // 1
    for (x <- Set(AtAtom(m(37.2),busG),busG,WDiamAtom(wop1,DiamAtom(request)),AtAtom(m(39.1),tramB), tramB)) {
      assert(strat(x) == 1)
    }
    // 0
    for (x <- Set(request)) {
      assert(strat(x) == 0)
    }

    val stratum: Map[Int, Program] = Strata(P)

    val P2 = Program(Set(r3))
    val P3 = Program(Set(r1g,r2g))
    val P5 = Program(Set(r4,r5))

    assert(stratum(0) == P2)
    assert(stratum(1) == P3)
    assert(stratum(2) == P5)

    //example for property 1:
    val ias: Set[S] = IAS(P,Dp,t)
    //println(ias)
    assert(AS(P,Dp,t) == ias)

    val performanceTest = false

    if (performanceTest) {

      def runtime(any: => Any): Double = {
        val s = System.currentTimeMillis()
        for (i <- 1 to 100) {
          any
        }
        var d = (System.currentTimeMillis() - s) / 1000.0
        println(d)
        return d
      }

      var runs = 5
      //
      var rt_as = 0.0
      runtime(AS(P, Dp, t)) //jvm opt
      for (i <- 1 to runs) {
        rt_as += runtime(AS(P, Dp, t))
      }
      var rt_ias = 0.0
      rt_ias += runtime(IAS(P, Dp, t))
      for (i <- 1 to runs) {
        rt_ias += runtime(IAS(P, Dp, t))
      }

      println("avg:")
      println("AS: " + (rt_as / (1.0 * runs)) + " sec")
      println("IAS: " + (rt_ias / (1.0 * runs)) + " sec")

    }

  }

}
