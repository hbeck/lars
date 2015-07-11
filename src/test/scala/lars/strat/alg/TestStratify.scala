package lars.strat.alg

import lars.core.semantics.formulas.{ExtendedAtom, Atom}
import lars.strat._
import org.scalatest.FunSuite

/**
 * Created by hb on 7/10/15.
 */
class TestStratify extends FunSuite {

  object x1 extends Atom
  object x2 extends Atom
  object y1 extends Atom
  object y2 extends Atom
  object z1 extends Atom
  object z2 extends Atom
  object w extends Atom

  val x1_x2 = e(x1,x2,geq)
  val x2_x1 = e(x2,x1,geq)
  val x1_y1 = e(x1,y1,geq)
  val y1_z1 = e(y1,z1,grt)
  val y1_z2 = e(y1,z2,geq)
  val x2_y2 = e(x2,y2,geq)
  val w_y2 = e(w,y2,grt)

  val nodes = Set[ExtendedAtom](x1,x2,y1,y2,z1,z2,w)
  val edges = Set[DepEdge](x1_x2,x2_x1,x1_y1,y1_z1,y1_z2,x2_y2,w_y2)

  val depGraph = DepGraph(nodes,edges)

  val sccs: Map[ExtendedAtom,DepGraph] = SCCs(depGraph)

  val c_x1x2 = sccs(x1)
  val c_y1 = sccs(y1)
  val c_y2 = sccs(y2)
  val c_z1 = sccs(z1)
  val c_z2 = sccs(z2)
  val c_w = sccs(w)

  test("SCC") {
    assert(sccs.keySet.size == 7)
    val g_x1x2 = g(Set(x1,x2),x1_x2,x2_x1)
    assert(c_x1x2 == g_x1x2)
    assert(sccs(x2) == g_x1x2)
    for (n <- Set(y1,y2,z1,z2,w)) {
      assert(sccs(n) == g(Set(n)))
    }
  }

  val cg = ComponentGraph(depGraph,sccs)

  test("component graph") {
    assert(cg.hasEdge(c_x1x2,c_y1))
    assert(cg.hasEdge(c_x1x2,c_y2))
    assert(cg.hasEdge(c_y1,c_z1))
    assert(cg.hasEdge(c_y1,c_z2))
    assert(cg.hasEdge(c_w,c_y2))

    assert(cg.maxStratum() == 2)

    val idx = cg.graph2stratum
    assert(idx(c_x1x2) == 2)
    assert(idx(c_y1) == 1)
    assert(idx(c_z1) == 0)
    assert(idx(c_z2) == 0)
    assert(idx(c_y2) == 1) //rethink - should it be 0?
    assert(idx(c_w) == 2)
  }

  val strat:Strat = Stratify.makeStrat(cg)

  test("makeStrat") {
    assert(strat(0) == Set(z1,z2))
    assert(strat(1) == Set(y1,y2))
    assert(strat(2) == Set(x1,x2,w))
    assert(strat.maxStratum == 2)
  }

  test("TestSCC objects stratified x y z") {
    var s: Strat = Stratify(g_x).get
    assert(s.maxStratum == 0)
    assert(s(0) == Set(x))
    //
    s = Stratify(g_xy_1).get
    assert(s.maxStratum == 1)
    assert(s(0) == Set(y))
    assert(s(1) == Set(x))
    //
    s = Stratify(g_xy_2).get
    assert(s.maxStratum == 0)
    assert(s(0) == Set(x,y))
    //
    //
    s = Stratify(g_xz).get
    assert(s.maxStratum == 0)
    assert(s(0) == Set(x,z))
    //
    s = Stratify(g_xyz_1).get
    assert(s.maxStratum == 1)
    assert(s(0) == Set(y,z))
    assert(s(1) == Set(x))
    //
    s = Stratify(g_xyz_2).get
    assert(s.maxStratum == 0)
    assert(s(0) == Set(x,y,z))
    //
    s = Stratify(g1).get
    assert(s.maxStratum == 0)
    assert(s(0) == Set(a,b,c,d,f))
  }

//  test("special") {
//    val s = Stratify(g2).get
//    assert(s.maxStratum == 1)
//    assert(s(1) == Set(a,b,c,d,f))
//    assert(s(0) == Set(h,i,j))
//  }

  test("no stratification") {
    val cycleG = g(Set(x,y),e(x,y,grt),e(y,x,grt))
    val s: Option[Strat] = Stratify(cycleG)
    assert(s.isEmpty)
  }

}
