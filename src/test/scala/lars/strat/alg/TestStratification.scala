package lars.strat.alg

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.extatoms.{AtAtom, WAt, WDiam}
import lars.core.semantics.programs.standard.{StdProgram, StdRule}
import lars.core.windowfn.time.TimeWindowFixedParams
import lars.graph.alg.SCCFn
import lars.graph.quotient.{Block, Condensation, QuotientGraph}
import lars.strat._
import org.scalatest.FunSuite

/**
 * Created by hb on 7/10/15.
 */
class TestStratification extends FunSuite {

  test("strat 1 - single parts") {

    val nodes1 = Set[ExtendedAtom](a1,a2,a3,wb,b,wc,c,wd1,d1,d2)
    val edges1 = Set[DepEdge[ExtendedAtom]](
      e(a3,a1,geq),e(a1,a2,geq),e(a2,a3,geq),
      e(a1,wb,geq),e(wb,b,grt),e(b,wc,geq),e(wc,c,grt),
      e(b,wd1,geq),e(wd1,d1,grt),e(d1,d2,eql),e(d2,d1,eql),
      e(a3,wd1,geq),e(a3,d2,geq)
      )
    val graph1 = DepGraph(nodes1,edges1)

    //
    val condensation: QuotientGraph[ExtendedAtom] = Condensation(graph1)
    def blk(ats:ExtendedAtom*) = Block(ats.toSet)
    val blockA = blk(a1,a2,a3)
    val blockWB = blk(wb)
    val blockB = blk(b)
    val blockWC = blk(wc)
    val blockC = blk(c)
    val blockWD1 = blk(wd1)
    val blockD = blk(d1,d2)
    assert(condensation.adjList.keys.size == 7)
    assert(condensation.adjList(blockA).contains(blockWB))
    assert(condensation.adjList(blockA).contains(blockWD1))
    assert(condensation.adjList(blockA).contains(blockD))
    assert(condensation.adjList(blockA).size == 3)
    assert(condensation.adjList(blockWB).contains(blockB))
    assert(condensation.adjList(blockWB).size == 1)
    assert(condensation.adjList(blockB).contains(blockWC))
    assert(condensation.adjList(blockB).contains(blockWD1))
    assert(condensation.adjList(blockB).size == 2)
    assert(condensation.adjList(blockWC).contains(blockC))
    assert(condensation.adjList(blockWC).size == 1)
    assert(condensation.adjList(blockC).isEmpty)
    assert(condensation.adjList(blockWD1).contains(blockD))
    assert(condensation.adjList(blockWD1).size == 1)
    assert(condensation.adjList(blockD).isEmpty)

    val condensationDepGraph: DepGraph[Block[ExtendedAtom]] = Stratification.createCondensationDepGraph(graph1,condensation)

    val expectedCDG = DepGraph[Block[ExtendedAtom]](
      Set(blockA,blockB,blockC,blockC,blockD,blockWB,blockWC,blockWD1),
      Set(DepEdge(blockA,blockWB,geq),DepEdge(blockA,blockWD1,geq),DepEdge(blockA,blockD,geq),
          DepEdge(blockWB,blockB,grt),DepEdge(blockB,blockWC,geq),DepEdge(blockB,blockWD1,geq),
          DepEdge(blockWC,blockC,grt),DepEdge(blockWD1,blockD,grt))
    )
    assert(condensationDepGraph == expectedCDG)

    val rootedDAG = Stratification.ensureRooted(condensationDepGraph)
    assert(rootedDAG == condensationDepGraph)

    val blockNumbering: Map[Block[ExtendedAtom], Int] = TopDownNumbering(rootedDAG)

    val expectedBN = Map(
      blockA -> 2, blockWB -> 2, blockB -> 1, blockWC -> 1, blockWD1 -> 1, blockC -> 0, blockD -> 0
    )
    assert(blockNumbering == expectedBN)

    val idxToAtoms: Map[Int, Set[ExtendedAtom]] = Stratification.createStratumMapping(blockNumbering)

    val expectedITA = Map[Int, Set[ExtendedAtom]](
      0 -> Set(c,d1,d2), 1 -> Set(wc,wd1,b), 2 -> Set(wb,a1,a2,a3)
    )
    assert(idxToAtoms == expectedITA)

    //final
    val expStrat = Stratification(expectedITA)
    assert(expStrat == Stratification(idxToAtoms))

  }

  test("strat 1 - program") {
    val wop = TimeWindowFixedParams(42).toOp()
    val program = StdProgram(Set(
      StdRule(a1, a3),StdRule(a2,a1),StdRule(a3,a2),StdRule(a1,WDiam(wop,b)),StdRule(b,WDiam(wop,c)),StdRule(b,WAt(wop,0,d))
    ))
    val optStrat: Option[Stratification] = Stratification(program)
    assert(optStrat.isDefined)
    val strat: Stratification = optStrat.get
    //println(strat)
    assert(strat.maxStratum == 2)
    assert(strat(0) == Set(c,d,AtAtom(0,d)))
    assert(strat(1) == Set(WDiam(wop,c),WAt(wop,0,d),b))
    assert(strat(2) == Set(WDiam(wop,b),a1,a2,a3))

    val stratum: Map[Int, StdProgram] = Strata(program)
    assert(stratum(0) == StdProgram())
    assert(stratum(1) == StdProgram(Set(StdRule(b,WDiam(wop,c)),StdRule(b,WAt(wop,0,d)))))
    assert(stratum(2) == StdProgram(Set(StdRule(a1, a3),StdRule(a2,a1),StdRule(a3,a2),StdRule(a1,WDiam(wop,b)))))
  }

  test("strat 2") {
    val nodes2 = Set[ExtendedAtom](a,b,c,d,x,f,y,h,wa,wb,wx,wf,wy,wh)
    val edges2 = Set[DepEdge[ExtendedAtom]](
      e(wa,a,grt),e(b,wa,geq),e(wb,b,grt),e(c,wb,geq),e(c,d,eql),e(d,c,eql),
      e(d,wx,geq),e(wx,x,grt),
      e(x,wf,geq),e(wf,f,grt),
      e(x,wy,geq),e(wy,y,grt),e(y,wh,geq),e(wh,h,grt)
    )
    val graph2 = DepGraph(nodes2,edges2)

    val strat = Stratification._apply(graph2).get

    assert(strat(a) == 0)
    assert(strat(wa) == 1)
    assert(strat(b) == 1)
    assert(strat(wb) == 2)
    assert(strat(c) == 3)
    assert(strat(d) == 3)
    assert(strat(wx) == 3)
    assert(strat(x) == 2)
    assert(strat(wf) == 1)
    assert(strat(f) == 0)
    assert(strat(wy) == 2)
    assert(strat(y) == 1)
    assert(strat(wh) == 1)
    assert(strat(h) == 0)
    assert(strat.maxStratum == 3)
  }

  //sample window defs; need only structural tests here
  val wop = TimeWindowFixedParams(42).toOp()
  def w(x:Atom) = WDiam(wop,x)
  def wat(x:Atom) = WAt(wop,0,x)
  def at(x:Atom) = AtAtom(0,x)
  def r(h: ExtendedAtom, x:ExtendedAtom) = new StdRule(h,Set(x))
  def p(xs:(ExtendedAtom,ExtendedAtom)*) = StdProgram( xs.map(pair => StdRule(pair._1,pair._2)).toSet )

  test("strat 2 - program") {
    val program = p( //rule in format of (head,body) pairs
      (c,d), (d,c), (c,w(b)), (b,w(a)), (d,w(x)), (x,w(y)), (x,w(f)), (y,w(h))
    )
    val optStrat: Option[Stratification] = Stratification(program)
    assert(optStrat.isDefined)
    val strat: Stratification = optStrat.get
    //println(strat)
    assert(strat.maxStratum == 3)
    assert(strat(0) == Set(a,h,f))
    assert(strat(1) == Set(w(a),b,w(h),y,w(f)))
    assert(strat(2) == Set(w(b),w(y),x))
    assert(strat(3) == Set(c,d,w(x)))

    val stratum = Strata(program)
    assert(stratum(0) == p())
    assert(stratum(1) == p((b,w(a)),(y,w(h))))
    assert(stratum(2) == p((x,w(y)),(x,w(f))))
    assert(stratum(3) == p((c,d),(d,c),(c,w(b)),(d,w(x))))
  }

  test("strat 3") {

    val x1_x2 = e(x1,x2,geq)
    val x2_x1 = e(x2,x1,geq)
    val x1_y1 = e(x1,y1,grt)
    val y1_z1 = e(y1,z1,grt)
    val y1_z2 = e(y1,z2,geq)
    val x2_y2 = e(x2,y2,geq)
    val w_y2 = e(w1,y2,grt)

    val nodes3 = Set[ExtendedAtom](x1,x2,y1,y2,z1,z2,w1)
    val edges3 = Set[DepEdge[ExtendedAtom]](x1_x2,x2_x1,x1_y1,y1_z1,y1_z2,x2_y2,w_y2)

    val depGraph3 = DepGraph(nodes3,edges3)

    val sccs: Map[ExtendedAtom,Block[ExtendedAtom]] = SCCFn[ExtendedAtom]()(depGraph3)

    val set_x1x2 = sccs(x1)
//    val set_y1 = sccs(y1)
//    val set_y2 = sccs(y2)
//    val set_z1 = sccs(z1)
//    val set_z2 = sccs(z2)
//    val set_w = sccs(w)

    //steps:
    assert(sccs.keySet.size == 7)
    val g_x1x2 = g(Set(x1,x2),x1_x2,x2_x1)
    assert(set_x1x2 == g_x1x2.nodes)
    assert(sccs(x2) == g_x1x2.nodes)
    for (n <- Set(y1,y2,z1,z2,w1)) {
      assert(sccs(n) == Set(n))
    }

    val condensation: QuotientGraph[ExtendedAtom] = Condensation(depGraph3)
    val condensationDepGraph: DepGraph[Block[ExtendedAtom]] = Stratification.createCondensationDepGraph(depGraph3,condensation)
    assert(!condensationDepGraph.isRooted)

    val rootedDAG = Stratification.ensureRooted(condensationDepGraph)
    assert(condensationDepGraph != rootedDAG)
    assert(rootedDAG.isRooted)

    val blockNumbering: Map[Block[ExtendedAtom], Int] = TopDownNumbering(rootedDAG)
    val idxToAtoms: Map[Int, Set[ExtendedAtom]] = Stratification.createStratumMapping(blockNumbering)
    val expITA = Map[Int,Set[ExtendedAtom]](
      0 -> Set(z1,z2,y2), 1 -> Set(y1,w1), 2 -> Set(x1,x2)
    )
    assert(idxToAtoms == expITA)

    //all at once:
    val strat = Stratification._apply(depGraph3).get
    assert(strat(0) == Set(z1,z2,y2))
    assert(strat(1) == Set(y1,w1))
    assert(strat(2) == Set(x1,x2))
    assert(strat.maxStratum == 2)
  }

  test("strat 3 variant - program") {
    val program = p( //head body pairs
      (z1,w(y2)), (x2,y2), (x2,x1), (x1,x2), (x1,w(x3))
    )
    val optStrat: Option[Stratification] = Stratification(program)
    assert(optStrat.isDefined)
    val strat: Stratification = optStrat.get
    //println(strat)
    assert(strat.maxStratum == 1)
    assert(strat(0) == Set(x3,y2))
    assert(strat(1) == Set(w(x3),x1,x2,w(y2),z1))

    val stratum = Strata(program)
    assert(stratum(0)==p())
    assert(stratum(1)==program)
  }

  test("strat 4") {
    val nodes = Set[ExtendedAtom](a1,a2,b,c,d,f)
    val depGraph = g(nodes, e(f,b,grt), e(f,d,grt), e(d,c,grt), e(c,b,grt), e(b,a1,grt), e(d,a2,grt))

    val s: Stratification = Stratification._apply(depGraph).get
    assert(s(a1)==0)
    assert(s(a2)==0)
    assert(s(b)==1)
    assert(s(c)==2)
    assert(s(d)==3)
    assert(s(f)==4)
  }

  test("strat 5 - no stratification") {
    val cycleG = g(Set(x,y),e(x,y,grt),e(y,x,grt))
    val s: Option[Stratification] = Stratification._apply(cycleG)
    assert(s.isEmpty)
  }

  test("strat 6") {

    val nodes = Set[ExtendedAtom](w(c),b,c,a)
    val edges = Set[DepEdge[ExtendedAtom]](e(a,c,geq),e(b,a,geq),e(b,w(c),geq),e(w(c),c,grt))

    val depGraph = DepGraph(nodes,edges)

    val strat = Stratification._apply(depGraph).get

    assert(strat(0) == Set(a,c))
    assert(strat(1) == Set(b,w(c)))

  }

  test("strat 6 - program") {
    val program = p( //head body pairs
      (b,w(c)), (b,a), (a,c)
    )
    val optStrat: Option[Stratification] = Stratification(program)
    assert(optStrat.isDefined)
    val strat: Stratification = optStrat.get
    //println(strat)
    assert(strat.maxStratum == 1)
    assert(strat(0) == Set(a,c))
    assert(strat(1) == Set(b,w(c)))

    val stratum = Strata(program)
    assert(stratum(0)==p((a,c)))
    assert(stratum(1)==p((b,w(c)),(b,a)))
  }

  test("strat 7"){

    val a_b = e(a,b,geq)
    val a_f = e(a,f,grt)
    val a_x = e(a,x,geq)
    val b_c = e(b,c,geq)
    val c_d = e(c,d,geq)
    val d_h = e(d,h,grt)
    val d_i = e(d,i,geq)
    val d_b = e(d,b,geq)
    val h_j = e(h,j,geq)
    val i_j = e(i,j,geq)
    val f_i = e(f,i,geq)
    val f_j = e(f,j,geq)
    val x_j = e(x,j,geq)

    val nodes = Set[ExtendedAtom](a,b,c,d,f,x,h,i,j)
        val edges = Set[DepEdge[ExtendedAtom]](a_b,a_f,a_x,b_c,c_d,d_h,d_i,d_b,h_j,i_j,f_i,f_j,x_j)

    val depGraph = DepGraph(nodes,edges)

    val strat = Stratification._apply(depGraph).get

    assert(strat(0) == Set(x,f,h,i,j))
    assert(strat(1) == Set(a,d,c,b))

  }

  test("strat 8 program") {
    val program = p( //head body pairs
      (x1,x2), (x2,x3), (x3,x1), (w2,at(z1)), (x2,wat(z1)), (w3,w(x3))
    ) + StdRule(w1,Set(w(x1),x3))
    val optStrat: Option[Stratification] = Stratification(program)
    assert(optStrat.isDefined)
    val strat: Stratification = optStrat.get
    //println(strat)
    assert(strat.maxStratum == 2)
    assert(strat(0) == Set(z1,at(z1),w2))
    assert(strat(1) == Set(wat(z1),x1,x2,x3))
    assert(strat(2) == Set(w(x1),w1,w(x3),w3))

    val stratum = Strata(program)
    assert(stratum(0)==p((w2,at(z1))))
    assert(stratum(1)==p((x1,x2), (x2,x3), (x3,x1), (x2,wat(z1))))
    assert(stratum(2)==(p((w3,w(x3)))) + StdRule(w1,Set(w(x1),x3)))
  }

  test("strat 9 disconnected 1") {
    val program = p((a,b),(c,d))
    val optStrat: Option[Stratification] = Stratification(program)
    assert(optStrat.isDefined)
    val strat: Stratification = optStrat.get
    //println(strat)
    assert(strat.maxStratum == 0)
    assert(strat(0) == Set(a,b,c,d))

    val stratum = Strata(program)
    assert(stratum(0)==program)
  }

  test("strat 10 disconnected 2") {
    val program =
      p((y1,w(x1)),(x1,x2),(x2,x1),(x1,z1),(x2,z2)) ++
      p((y2,wat(a)),(a,b),(at(a),w(b)))

    val optStrat: Option[Stratification] = Stratification(program)
    assert(optStrat.isDefined)
    val strat: Stratification = optStrat.get
    //println(strat)
    assert(strat.maxStratum == 2)
    assert(strat(0) == Set(z1,z2,x1,x2,b))
    assert(strat(1) == Set(w(x1),y1,a,at(a),w(b)))
    assert(strat(2) == Set(wat(a),y2))

    val stratum = Strata(program)
    assert(stratum(0)==p((x1,x2),(x2,x1),(x1,z1),(x2,z2)))
    assert(stratum(1)==p((y1,w(x1)),(a,b),(at(a),w(b))))
    assert(stratum(2)==p((y2,wat(a))))
  }

}
