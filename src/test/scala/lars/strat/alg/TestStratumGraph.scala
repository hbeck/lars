package lars.strat.alg

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.graph.alg.{DepPartition, BottomUpNumbering, SCCFn}
import lars.graph.quotient.Condensation
import lars.strat._
import org.scalatest.FunSuite

/**
 * Created by hb on 7/10/15.
 */
class TestStratumGraph extends FunSuite {

  object a extends Atom
  object b extends Atom
  object c extends Atom
  object d extends Atom


  val a_c = e(a,c,geq)
  val b_a = e(b,a,geq)
  val b_d = e(b,d,geq)
  val d_c = e(d,c,grt)

//  val nodes = Set[ExtendedAtom](a,b,c,d)
  val nodes = Set[ExtendedAtom](a,b,d,c) //testthingy
//  val nodes = Set[ExtendedAtom](a,b,d,c)
//  val nodes = Set[ExtendedAtom](a,d,b,c)
//  val nodes = Set[ExtendedAtom](d,a,b,c)
//  val nodes = Set[ExtendedAtom](d,a,c,b)
//  val nodes = Set[ExtendedAtom](d,c,a,b)
//  val nodes = Set[ExtendedAtom](c,d,a,b)
//  val nodes = Set[ExtendedAtom](c,d,b,a)
//  val nodes = Set[ExtendedAtom](c,b,d,a)
//  val nodes = Set[ExtendedAtom](b,c,d,a)
//  val nodes = Set[ExtendedAtom](b,c,a,d)
//  val nodes = Set[ExtendedAtom](b,a,c,d)
//  val nodes = Set[ExtendedAtom](d,c,b,a)
//  val nodes = Set[ExtendedAtom](d,c,a,b)
//  val nodes = Set[ExtendedAtom](d,a,c,b)
//  val nodes = Set[ExtendedAtom](a,d,c,b)
//  val nodes = Set[ExtendedAtom](a,d,b,c)
//  val nodes = Set[ExtendedAtom](a,b,d,c)
//  val nodes = Set[ExtendedAtom](a,c,b,d)
//  val nodes = Set[ExtendedAtom](c,a,b,d)
//  val nodes = Set[ExtendedAtom](c,a,d,b)
//  val nodes = Set[ExtendedAtom](d,b,c,a)
//  val nodes = Set[ExtendedAtom](d,b,a,c) //testthingy
//  val nodes = Set[ExtendedAtom](d,a,b,c)

    val edges = Set[DepEdge](a_c,b_a,b_d,d_c)

    val depGraph = DepGraph(nodes,edges)

    val stratg: Map[ExtendedAtom,Set[ExtendedAtom]] = DepPartition()(depGraph)

/*  val set_abd = stratg(b)
  val set_c = stratg(c)

  val set_ac = stratg(a)
  val set_bd = stratg(d)*/

//  assert((set_abd && set_c) || (set_ac && set_bd))
/*  println(set_abd)
  println(set_c)

  println("---")

  println(set_ac)
  println(set_bd)*/


  /*test("SCC") {
    assert(sccs.keySet.size == 7)
    val g_x1x2 = g(Set(x1,x2),x1_x2,x2_x1)
    assert(set_x1x2 == g_x1x2.nodes)
    assert(sccs(x2) == g_x1x2.nodes)
    for (n <- Set(y1,y2,z1,z2,w)) {
      assert(sccs(n) == Set[ExtendedAtom](n))
    }
  }

  val con = Condensation(depGraph)

  test("component graph") {
    assert(con.hasEdge(set_x1x2,set_y1))
    assert(con.hasEdge(set_x1x2,set_y2))
    assert(con.hasEdge(set_y1,set_z1))
    assert(con.hasEdge(set_y1,set_z2))
    assert(con.hasEdge(set_w,set_y2))

    val idx: Map[Set[ExtendedAtom],Int] = BottomUpNumbering(con)

    val maxStratum = idx.values.reduce(math.max)

    assert(maxStratum == 2)

    //val stratumToAtoms: Map[Int, Set[ExtendedAtom]] = idx.toSeq.map(_.swap).map( pair => (pair._1, pair._2.nodes) ).toMap

    assert(idx(set_x1x2) == 2)
    assert(idx(set_y1) == 1)
    assert(idx(set_z1) == 0)
    assert(idx(set_z2) == 0)
    assert(idx(set_y2) == 0)
    assert(idx(set_w) == 1)
  }

  val strat:Stratification = Stratification(Stratify.createStratumMapping(BottomUpNumbering(con)))

  test("makeStrat") {
    assert(strat(0) == Set[ExtendedAtom](z1,z2,y2))
    assert(strat(1) == Set[ExtendedAtom](y1,w))
    assert(strat(2) == Set[ExtendedAtom](x1,x2))
    assert(strat.maxStratum == 2)
  }

  test("TestSCC objects stratified x y z") {
    var s: Stratification = Stratify(g_x).get
    assert(s.maxStratum == 0)
    assert(s(0) == Set[ExtendedAtom](x))
    //
    s = Stratify(g_xy_1).get
    assert(s.maxStratum == 1)
    assert(s(0) == Set[ExtendedAtom](y))
    assert(s(1) == Set[ExtendedAtom](x))
    //
    s = Stratify(g_xy_2).get
    assert(s.maxStratum == 0)
    assert(s(0) == Set[ExtendedAtom](x,y))
    //
    //
    s = Stratify(g_xz).get
    assert(s.maxStratum == 0)
    assert(s(0) == Set[ExtendedAtom](x,z))
    //
    s = Stratify(g_xyz_1).get
    assert(s.maxStratum == 1)
    assert(s(0) == Set[ExtendedAtom](y,z))
    assert(s(1) == Set[ExtendedAtom](x))
    //
    s = Stratify(g_xyz_2).get
    assert(s.maxStratum == 0)
    assert(s(0) == Set[ExtendedAtom](x,y,z))
    //
    //
    s = Stratify(g1).get
    assert(s.maxStratum == 0)
    assert(s(0) == Set[ExtendedAtom](a,b,c,d,f))
    //
    s = Stratify(g2).get
    assert(s.maxStratum == 1)
    assert(s(1) == Set[ExtendedAtom](a,b,c,d,f))
    assert(s(0) == Set[ExtendedAtom](h,i,j))
  }

  test("bottom up numbering handling geq cycle") {
    object a1 extends Atom
    object a2 extends Atom
    val nodes = Set[ExtendedAtom](a1,a2,b,c,d,f)
    val depGraph = g(nodes, e(f,b,grt), e(f,d,grt), e(d,c,grt), e(c,b,grt), e(b,a1,grt), e(d,a2,grt))
    val s: Stratification = Stratify(depGraph).get
    assert(s(a1)==0)
    assert(s(a2)==0)
    assert(s(b)==1)
    assert(s(c)==2)
    assert(s(d)==3)
    assert(s(f)==4)
  }

  test("no stratification") {
    val cycleG = g(Set(x,y),e(x,y,grt),e(y,x,grt))
    val s: Option[Stratification] = Stratify(cycleG)
    assert(s.isEmpty)
  }*/

}
