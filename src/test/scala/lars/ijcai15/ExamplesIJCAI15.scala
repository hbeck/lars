package lars.ijcai15

import lars.core.semantics.formulas._
import lars.core.semantics.programs._
import lars.core.semantics.programs.extatoms.{AtAtom, ExtendedAtoms, WAt, WDiam}
import lars.core.semantics.programs.general.{GeneralProgram, GeneralRule}
import lars.core.semantics.programs.standard.{StdProgram, StdRule}
import lars.core.semantics.streams.{Evaluation, S, Timeline}
import lars.core.semantics.structure.IsAnswerStream
import lars.core.windowfn.time.{TimeWindow, TimeWindowFixedParams, TimeWindowParameters}
import lars.strat._
import lars.tms.cons.{ConsAt, ConsH, ConsW}
import lars.tms.status.Status.in
import lars.tms.status.rule.fVal
import lars.tms.status.{Label, Labels}
import lars.util.map.Merge
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
    val fm2 = WindowFormula(wop3, sfm) //alt
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
  val r1g_gen = GeneralRule(At(m(37.2)+m(3),expBusM), WindowFormula(wop3,At(m(37.2),busG)) and on)
  val r2g_gen = GeneralRule(At(m(39.1)+m(5),expTrM), WindowFormula(wop5,At(m(39.1),tramB)) and on)
  val r3_gen = GeneralRule(on, WindowFormula(wop1,Diam(request)))
  val r4_gen = GeneralRule(takeBusM, WindowFormula(wopP5,Diam(expBusM)) and Not(takeTrM) and Not(WindowFormula(wop3,Diam(jam))))
  val r5_gen = GeneralRule(takeTrM, WindowFormula(wopP5,Diam(expTrM)) and Not(takeBusM))
//  for (rule <- Set (r1g,r2g,r3,r4,r5)) {
//    println(rule)
//  }
  val r1g = StdRule(AtAtom(m(37.2)+m(3),expBusM), Set(WAt(wop3,m(37.2),busG),on))
  val r2g = StdRule(AtAtom(m(39.1)+m(5),expTrM), WAt(wop5,m(39.1),tramB), on) //convenience variant
  val r3 = StdRule(on, WDiam(wop1,request))
  val r4 = StdRule(takeBusM, WDiam(wopP5,expBusM), Not(takeTrM), Not(WDiam(wop3,jam)))
  val r5 = StdRule(takeTrM, WDiam(wopP5,expTrM), Not(takeBusM))
  //
  val PGen = GeneralProgram(Set(r1g_gen,r2g_gen,r3_gen,r4_gen,r5_gen))
  val P = StdProgram(Set(r1g,r2g,r3,r4,r5))
  val t = m(39.7)
  val Dp = D + (t -> request)

  test("ex6 general program") {
    val common = Map[Int,Set[Atom]](m(40.2) -> Set(expBusM), m(44.1) -> Set(expTrM), t -> Set(on))
    val mI1 = Merge(Map[Int,Set[Atom]](t -> Set(takeTrM)), common)
    val mI2 = Merge(Map[Int,Set[Atom]](t -> Set(takeBusM)), common)
    //
    val I1 = Dp ++ S(T,Evaluation(mI1))
    val I2 = Dp ++ S(T,Evaluation(mI2))
    //
    // steps for i1
    val m1 = I1.toStructure(Set[Atom]())
    assert((m1/t |= r1g_gen.body))
    assert((m1/t |= r2g_gen.body))
    assert((m1/t |= r3_gen.body))
    assert((m1/t |= r4_gen.body) == false)
    //
    assert((m1/m(44.1) |= expTrM))
    assert((m1/m(44.1) |= Diam(expTrM)))
    assert((m1/m(0) |= Diam(expTrM)))
    assert((m1/m(50) |= Diam(expTrM)))
    assert((m1/m(44.1) |= WindowFormula(wopP5,expTrM)))
    assert((m1/m(44.1) |= WindowFormula(wopP5,Diam(expTrM))))
    assert((m1/(m(44.1)+1) |= WindowFormula(wopP5,Diam(expTrM))) == false)
    assert((m1/(m(44.1)-1) |= WindowFormula(wopP5,Diam(expTrM))))
    assert((m1/(m(39.1)) |= WindowFormula(wopP5,Diam(expTrM))))
    assert((m1/(m(39.1)-1) |= WindowFormula(wopP5,Diam(expTrM))) == false)
    assert((m1/t |= takeBusM) == false)
    assert((m1/t |= Not(takeBusM)))
    //
    assert((m1/t |= r5_gen.body))
    //
    val reductRules = Set[GeneralRule](r1g_gen,r2g_gen,r3_gen,r5_gen);
    assert(PGen.rules.filter(m1/t |= _.body) == reductRules) //note: .sameElements also checks order
    //
    val PR1 = Reduct(PGen,m1,t)
    assert(PR1.rules == reductRules)
    //manual model check for all rules of the reduct
    //r1g: At(m(37.2)+m(3),expBusM), w3(At(m(37.2),busG)) and on
    assert(m1/t |= WindowFormula(wop3,At(m(37.2),busG)))
    assert(m1/t |= on)
    assert(m1/t |= At(m(37.2)+m(3),expBusM))
    assert(m1/t |= Implies(And(WindowFormula(wop3,At(m(37.2),busG)),on),At(m(37.2)+m(3),expBusM)))
    //
    assert(m1/t |= r1g_gen)
    assert(m1/t |= r2g_gen)
    assert(m1/t |= r3_gen)
    assert(m1/t |= r5_gen)
    //
    assert(m1.isMinimalModel(PR1,t,Dp))
    //
    //
    assert(IsAnswerStream(I1,PGen,Dp,t))
    assert(IsAnswerStream(I2,PGen,Dp,t))
    //
    var X = Dp ++ S(T,Evaluation(common))
    assert(IsAnswerStream(X,PGen,Dp,t) == false) //t -> takeTrM (or t -> takeBus) missing
    X = I1 + (t -> expTrM)
    assert(IsAnswerStream(X,PGen,Dp,t) == false) //non minimal
    X = I1 - (m(44.1) -> expTrM)
    assert(IsAnswerStream(X,PGen,Dp,t) == false) //m(44.1) -> expTrM missing
    //
    //
    //using all atoms yields a model
    assert((I1 ++ I2).toStructure(Set()).isModel(PGen,t))
    //
    val answerStreams = AS(PGen,Dp,t)
    assert(answerStreams.size == 2)
//    println("answer streams: "+answerStreams.size)
//    for (as <- answerStreams) {
//      println(as)
//    }
  }

  test("ex6 std program") {
    val common = Map[Int,Set[Atom]](m(40.2) -> Set(expBusM), m(44.1) -> Set(expTrM), t -> Set(on))
    val mI1 = Merge(Map[Int,Set[Atom]](t -> Set(takeTrM)), common)
    val mI2 = Merge(Map[Int,Set[Atom]](t -> Set(takeBusM)), common)
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
    assert((m1/m(44.1) |= WDiam(wopP5,expTrM)))
    assert((m1/(m(44.1)+1) |= WDiam(wopP5,expTrM)) == false)
    assert((m1/(m(44.1)-1) |= WDiam(wopP5,expTrM)))
    assert((m1/(m(39.1)) |= WDiam(wopP5,expTrM)))
    assert((m1/(m(39.1)-1) |= WDiam(wopP5,expTrM)) == false)
    //
    assert((m1/t |= r5.body))
    //
    val reductRules = Set[StdRule](r1g,r2g,r3,r5);
    assert(P.rules.filter(m1/t |= _.body) == reductRules) //note: .sameElements also checks order
    //
    val PR1 = Reduct(P,m1,t)
    assert(PR1.rules == reductRules)
    //manual model check for all rules of the reduct
    //r1g: At(m(37.2)+m(3),expBusM), w3(At(m(37.2),busG)) and on
    assert(m1/t |= WAt(wop3,m(37.2),busG))
    assert(m1/t |= on)
    assert(m1/t |= AtAtom(m(37.2)+m(3),expBusM))
    assert(m1/t |= Implies(And(WAt(wop3,m(37.2),busG),on),AtAtom(m(37.2)+m(3),expBusM)))
    //
    assert(m1/t |= r1g)
    assert(m1/t |= r2g)
    assert(m1/t |= r3)
    assert(m1/t |= r5)
    //
    assert(m1.isMinimalModel(PR1,t,Dp))
    //
    //
    assert(IsAnswerStream(I1,P,Dp,t))
    assert(IsAnswerStream(I2,P,Dp,t))
    //
    var X = Dp ++ S(T,Evaluation(common))
    assert(IsAnswerStream(X,P,Dp,t) == false) //t -> takeTrM (or t -> takeBus) missing
    X = I1 + (t -> expTrM)
    assert(IsAnswerStream(X,P,Dp,t) == false) //non minimal
    X = I1 - (m(44.1) -> expTrM)
    assert(IsAnswerStream(X,P,Dp,t) == false) //m(44.1) -> expTrM missing
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
  val PpGen = GeneralProgram(Set(GeneralRule(AtAtom(t,x),WAt(w3,t,y))))
  val Pp = StdProgram(Set(StdRule(AtAtom(t,x),WAt(w3,t,y))))

  test("ex7") {
    val expectedEAtoms: Set[ExtendedAtom] = Set(AtAtom(t,x),x,WAt(w3,t,y),AtAtom(t,y),y)
    val actualEAtoms: Set[ExtendedAtom] = ExtendedAtoms(Pp,true)
    assert(actualEAtoms == expectedEAtoms)
    //
    def e(from:ExtendedAtom, to:ExtendedAtom, d:Dependency) = DepEdge(from, to, d)
    val nodes = ExtendedAtoms(Pp,true)
    val expectedDepGraph = DepGraph(nodes,Set[DepEdge](
      e(AtAtom(t,x),WAt(w3,t,y),geq),
      e(WAt(w3,t,y),y,grt),
      e(AtAtom(t,x),x,eql),
      e(x,AtAtom(t,x),eql),
      e(AtAtom(t,y),y,eql),
      e(y,AtAtom(t,y),eql)
    ))
    val actualDepGraph = DepGraph(Pp)
    //    for (e <- actualDepGraph.edges) {
    //      println(e)
    //    }
    for ((x,y) <- actualDepGraph.edges) {
      assert(expectedDepGraph.edges contains (x,y))
    }
    for ((x,y) <- expectedDepGraph.edges) {
      assert(actualDepGraph.edges contains (x,y))
    }
    assert(actualDepGraph.edges == expectedDepGraph.edges)
    assert(actualDepGraph == expectedDepGraph)
  }

  test("ex8") {
    val program = Pp
    val opt: Option[Stratification] = Stratification(program)
    assert(opt.isDefined)
    val strat = opt.get
    assert(strat.maxStratum == 2)
    assert(strat(0) == Set(AtAtom(t,y),y))
    assert(strat(1) == Set(WAt(w3,t,y)))
    assert(strat(2) == Set(AtAtom(t,x),x))
    // can call also in direction as given in paper:
    assert(strat(AtAtom(t,y)) == 0)
    assert(strat(y) == 0)
    assert(strat(WAt(w3,t,y)) == 1)
    assert(strat(AtAtom(t,x)) == 2)
    assert(strat(x) == 2)
    // decision was made to have a 'maximal' stratification in the algorithm
    // however, ex8 as presented in the paper is also a stratification:
    assert(
      Stratification.isStratification(Map(
      AtAtom(t,y) -> 0,
      y -> 0,
      WAt(w3,t,y) -> 1,
      AtAtom(t,x) -> 1,
      x -> 1),
      program))
    //
    assert(Strata(program)(0)==program)
  }

  /*
  val r1g = Rule(At(m(37.2)+m(3),expBusM), Wop(wop3,At(m(37.2),busG)) and on)
  val r2g = Rule(At(m(39.1)+m(5),expTrM), Wop(wop5,At(m(39.1),tramB)) and on)
  val r3 = Rule(on, Wop(wop1,Diam(request)))
  val r4 = Rule(takeBusM, Wop(wopP5,Diam(expBusM)) and Not(takeTrM) and Not(Wop(wop3,Diam(jam))))
  val r5 = Rule(takeTrM, Wop(wopP5,Diam(expTrM)) and Not(takeBusM))
  */

  val ruleMapGen = Map[Int,GeneralRule]()+(1 -> r1g_gen)+(2 -> r2g_gen)+(3 -> r3_gen)+(4 -> r4_gen)+(5 -> r5_gen)
  val ruleMapStd = Map[Int,StdRule]()+(1 -> r1g)+(2 -> r2g)+(3 -> r3)+(4 -> r4)+(5 -> r5)

  // TODO create new test with new minimal stratification (that is not using StrongComponentGraph)
  test("ex9") {
    val program = P
    val extendedAtoms: Set[ExtendedAtom] = ExtendedAtoms(program,true)
    val expectedExtendedAtoms = Set(
      AtAtom(m(37.2)+m(3),expBusM), expBusM, WAt(wop3,m(37.2),busG), AtAtom(m(37.2),busG), busG, on,
      AtAtom(m(39.1)+m(5),expTrM), expTrM, WAt(wop5,m(39.1),tramB), AtAtom(m(39.1),tramB), tramB,
      WDiam(wop1,request), request,
      takeBusM, WDiam(wopP5,expBusM), takeTrM, WDiam(wop3,jam), jam,
      WDiam(wopP5,expTrM), expTrM
    )
    assert(extendedAtoms == expectedExtendedAtoms)
    //
    val strat = Stratification(program).get
    //
    assert(strat.maxStratum == 5)
    // 5
    for (x <- Set(takeBusM, takeTrM)) {
      assert(strat(x) == 5)
    }
    // 4
    for (x <- Set(WDiam(wopP5,expBusM),WDiam(wopP5,expTrM))) {
      assert(strat(x) == 4)
    }
    // 3
    for (x <- Set(AtAtom(m(37.2)+m(3),expBusM), expBusM, AtAtom(m(39.1)+m(5),expTrM), expTrM)) {
      assert(strat(x) == 3)
    }
    // 2
    for (x <- Set(on)) {
      assert(strat(x) == 2)
    }
    // 1
    for (x <- Set(WDiam(wop1,request),WAt(wop5,m(39.1),tramB),WAt(wop3,m(37.2),busG),WDiam(wop3,jam))) {
      assert(strat(x) == 1)
    }
    // 0
    for (x <- Set(request,AtAtom(m(37.2),busG),busG,jam,AtAtom(m(39.1),tramB), tramB)) {
      assert(strat(x) == 0)
    }

    val stratum: Map[Int, StdProgram] = Strata(program)

    val P2 = program(Set(r3))
    val P3 = program(Set(r1g,r2g))
    val P5 = program(Set(r4,r5))

    assert(stratum(0) == P2)
    assert(stratum(1) == P3)
    assert(stratum(2) == P5)

    //example for property 1:
    val ias: Set[S] = IAS(program,Dp,t)

//    println("\nAS: ")
//    println(AS(program,Dp,t))
//    println("\nias: ")
//    println(ias)

    assert(AS(program,Dp,t) == ias)

    val performanceTest = false

    if (performanceTest) {

      def runtime(any: => Any): Double = {
        val s = System.currentTimeMillis()
        for (i <- 1 to 100) {
          any
        }
        val d = (System.currentTimeMillis() - s) / 1000.0
        println(d)
        return d
      }

      val runs = 5
      //
      var rt_as = 0.0
      runtime(AS(program, Dp, t)) //jvm opt
      for (i <- 1 to runs) {
        rt_as += runtime(AS(program, Dp, t))
      }
      var rt_ias = 0.0
      rt_ias += runtime(IAS(program, Dp, t))
      for (i <- 1 to runs) {
        rt_ias += runtime(IAS(program, Dp, t))
      }

      println("avg:")
      println("AS: " + (rt_as / (1.0 * runs)) + " sec")
      println("IAS: " + (rt_ias / (1.0 * runs)) + " sec")

    }
  }

  test("ex10") {
    val P10 = StdProgram(Set[StdRule](r2g))
    assert(ConsW(P10,AtAtom(m(39.1),tramB)) == Set(WAt(wop5,m(39.1),tramB)))
    assert(ConsH(P10,WAt(wop5,m(39.1),tramB)) == Set(AtAtom(m(44.1),expTrM)))
    assert(ConsH(P10,on) == Set(AtAtom(m(44.1),expTrM)))
    assert(ConsAt(P10,AtAtom(m(44.1),expTrM)) == Set(expTrM))

    val mMap = new collection.mutable.HashMap[ExtendedAtom,Label]()
    mMap(WAt(wop5,m(39.1),tramB))=Label(in, (m(39.1),m(44.1)))
    mMap(on)=Label(in,(m(39.7),m(40.7)))
    val L=Labels(mMap)

    assert(fVal(L,r2g))
    //TODO these things will only be available after the AnswerUpdate:
//    assert(SuppP(P10,L,AtAtom(m(44.1),expTrM)) == Set(WAt(wop5,m(39.1),tramB), on))
//    assert(ACons(P10,L,WAt(wop5,m(39.1),tramB)) == Set(AtAtom(m(44.1),expTrM)))
  }

}
