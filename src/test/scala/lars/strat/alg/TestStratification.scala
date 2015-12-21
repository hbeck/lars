package lars.strat.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.graph.alg.SCCFn
import lars.graph.quotient.{QuotientGraph, Block, Condensation}
import lars.strat._
import org.scalatest.FunSuite

/**
 * Created by hb on 7/10/15.
 */
class TestStratification extends FunSuite {

  test("strat1 - single parts") {

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

    val expectedITA = Map(
      0 -> Set(c,d1,d2), 1 -> Set(wc,wd1,b), 2 -> Set(wb,a1,a2,a3)
    )
    assert(idxToAtoms == expectedITA)
  }


  val x1_x2 = e(x1,x2,geq)
  val x2_x1 = e(x2,x1,geq)
  val x1_y1 = e(x1,y1,geq)
  val y1_z1 = e(y1,z1,grt)
  val y1_z2 = e(y1,z2,geq)
  val x2_y2 = e(x2,y2,geq)
  val w_y2 = e(w,y2,grt)

  val nodes = Set[ExtendedAtom](x1,x2,y1,y2,z1,z2,w)
  val edges = Set[DepEdge[ExtendedAtom]](x1_x2,x2_x1,x1_y1,y1_z1,y1_z2,x2_y2,w_y2)

  val depGraph = DepGraph(nodes,edges)

  val sccs: Map[ExtendedAtom,Block[ExtendedAtom]] = SCCFn[ExtendedAtom]()(depGraph)

  val set_x1x2 = sccs(x1)
  val set_y1 = sccs(y1)
  val set_y2 = sccs(y2)
  val set_z1 = sccs(z1)
  val set_z2 = sccs(z2)
  val set_w = sccs(w)

  test("SCC") {
    assert(sccs.keySet.size == 7)
    val g_x1x2 = g(Set(x1,x2),x1_x2,x2_x1)
    assert(set_x1x2 == g_x1x2.nodes)
    assert(sccs(x2) == g_x1x2.nodes)
    for (n <- Set(y1,y2,z1,z2,w)) {
      assert(sccs(n) == Set(n))
    }
  }

  val con = Condensation(depGraph)

//  test("component graph") {
//    assert(con.hasEdge(set_x1x2,set_y1))
//    assert(con.hasEdge(set_x1x2,set_y2))
//    assert(con.hasEdge(set_y1,set_z1))
//    assert(con.hasEdge(set_y1,set_z2))
//    assert(con.hasEdge(set_w,set_y2))
//
//    val idx: Map[ExtendedAtom,Int] = TopDownNumbering(con)
//
//    val maxStratum = idx.values.reduce(math.max)
//
//    assert(maxStratum == 2)
//
//    //val stratumToAtoms: Map[Int, Set[ExtendedAtom]] = idx.toSeq.map(_.swap).map( pair => (pair._1, pair._2.nodes) ).toMap
//
//    assert(idx(set_x1x2) == 2)
//    assert(idx(set_y1) == 1)
//    assert(idx(set_z1) == 0)
//    assert(idx(set_z2) == 0)
//    assert(idx(set_y2) == 0)
//    assert(idx(set_w) == 1)
//  }

//  val strat:Stratification = Stratification.createStratumMapping(BottomUpNumbering(con))
//
//  test("makeStrat") {
//    assert(strat(0) == Set(z1,z2,y2))
//    assert(strat(1) == Set(y1,w))
//    assert(strat(2) == Set(x1,x2))
//    assert(strat.maxStratum == 2)
//  }

  //
//  test("TestSCC objects stratified x y z with identity fn") {
//    val f = { x:QuotientedGraph[...] => x } //identify function
//    var s: Stratification = Stratify(g_x,f).get
//    assert(s.maxStratum == 0)
//    assert(s(0) == Set(x))
//    //
//    s = Stratify(g_xy_1,f).get
//    assert(s.maxStratum == 1)
//    assert(s(0) == Set(y))
//    assert(s(1) == Set(x))
//    //
//    s = Stratify(g_xy_2,f).get
//    assert(s.maxStratum == 0)
//    assert(s(0) == Set(x,y))
//    //
//    //
//    s = Stratify(g_xz,f).get
//    assert(s.maxStratum == 0)
//    assert(s(0) == Set(x,z))
//    //
//    s = Stratify(g_xyz_1,f).get
//    assert(s.maxStratum == 1)
//    assert(s(0) == Set(y,z))
//    assert(s(1) == Set(x))
//    //
//    s = Stratify(g_xyz_2,f).get
//    assert(s.maxStratum == 0)
//    assert(s(0) == Set(x,y,z))
//    //
//    //
//    s = Stratify(g1,f).get
//    assert(s.maxStratum == 0)
//    assert(s(0) == Set(a,b,c,d,f))
//    //
//    s = Stratify(g2,f).get
//    assert(s.maxStratum == 1)
//    assert(s(1) == Set(a,b,c,d,f))
//    assert(s(0) == Set(h,i,j))
//  }

  //TODO: analogous to test("TestSCC objects stratified x y z with identity fn"),
  //create another test method where fn=extending function based on condensation,
  //adapt numbers to minimal stratification accordingly

  //TODO: note that current tests fail due to this reason that a condensation-based
  //stratification numbering is used. keep them (using identity function), add further copies for new one

  test("a1 a2 ..") {
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

  test("no stratification") {
    val cycleG = g(Set(x,y),e(x,y,grt),e(y,x,grt))
    val s: Option[Stratification] = Stratification._apply(cycleG)
    assert(s.isEmpty)
  }

  //TODO



  //  test("test1"){
  //
  //    val a_c = e(a,c,geq)
  //    val b_a = e(b,a,geq)
  //    val b_d = e(b,d,geq)
  //    val d_c = e(d,c,grt)
  //
  //    val nodes = Set[ExtendedAtom](d,b,c,a)
  //    val edges = Set[DepEdge[ExtendedAtom]](a_c,b_a,b_d,d_c)
  //
  //    val depGraph = DepGraph(nodes,edges)
  //
  //    //val quot = StratumGraph(depGraph).adjList //TODO
  //
  //    val set_abd_c = Map(Set(c)->Set(),Set(a,b,d)->Set(Set(c)))
  //    val set_ac_bd = Map(Set(a,c)->Set(), Set(b,d)->Set(Set(a,c)))
  //
  //    //assert(quot == set_abd_c || quot == set_ac_bd) //TODO
  //  }
  //
  //  test("test2"){
  //
  //    val a_b = e(a,b,geq)
  //    val a_f = e(a,f,grt)
  //    val a_g = e(a,g,geq)
  //    val b_c = e(b,c,geq)
  //    val c_d = e(c,d,geq)
  //    val d_h = e(d,h,grt)
  //    val d_i = e(d,i,geq)
  //    val d_b = e(d,b,geq)
  //    val h_j = e(h,j,geq)
  //    val i_j = e(i,j,geq)
  //    val f_i = e(f,i,geq)
  //    val f_j = e(f,j,geq)
  //    val g_j = e(g,j,geq)
  //
  //    val nodes = Set[ExtendedAtom](a,b,c,d,f,g,h,i,j)
  ////    println(nodes)
  //    val edges = Set[DepEdge](a_b,a_f,a_g,b_c,c_d,d_h,d_i,d_b,h_j,i_j,f_i,f_j,g_j)
  //
  //    val depGraph = DepGraph(nodes,edges)
  //
  //    val quot = StratumGraph(depGraph).adjList
  //
  ////    println("\n\nfinal:")
  ////    println(quot)
  //
  //    for ((x,y) <- Seq((a,f),(d,h),(a,h))) {
  //      for (block <- quot.keys) {
  //        if (block.contains(x)) {
  //          assert(!block.contains(y))
  //        }
  //        if (block.contains(y)) {
  //          assert(!block.contains(x))
  //        }
  //      }
  //    }
  //
  //    for (block <- quot.keys){
  //      if (block.contains(c)) {
  //        assert(block.contains(b))
  //        assert(block.contains(d))
  //      }
  //    }
  //
  //  }
  //
  //  test("test3"){
  //    object x1 extends Atom
  //    object x2 extends Atom
  //    object y1 extends Atom
  //    object y2 extends Atom
  //    object z1 extends Atom
  //    object z2 extends Atom
  //    object w extends Atom
  //
  //    val x1_x2 = e(x1,x2,geq)
  //    val x2_x1 = e(x2,x1,geq)
  //    val x1_y1 = e(x1,y1,geq)
  //    val y1_z1 = e(y1,z1,grt)
  //    val y1_z2 = e(y1,z2,geq)
  //    val x2_y2 = e(x2,y2,geq)
  //    val w_y2  = e(w,y2,grt)
  //
  //    val nodes = Set[ExtendedAtom](x1,x2,y1,y2,z1,z2,w)
  ////    println(nodes)
  //    val edges = Set[DepEdge](x1_x2,x2_x1,x1_y1,y1_z1,y1_z2,x2_y2,w_y2)
  //
  //    val depGraph = DepGraph(nodes,edges)
  //
  //    val quot = StratumGraph(depGraph).adjList
  ////    println(quot)
  //
  //    for (block <- quot.keys) {
  //      if (block.contains(w) || block.contains(z1)) {
  //        assert(block.size==1)
  //      } else {
  //        assert(block.size==5)
  //      }
  //    }
  //  }

}
