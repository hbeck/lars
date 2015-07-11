package lars.strat.alg

import lars.core.semantics.formulas.Atom
import lars.strat.{eql, geq, DepEdge, DepGraph}
import org.scalatest.FunSuite

/**
 * Created by hb on 7/11/15.
 */
class TestSCCs extends FunSuite {

  object x extends Atom
  object y extends Atom
  object z extends Atom

  test("test x y z") {
    val g_xy_1 = DepGraph(Set(x),Set())
    assert(SCCs(g_xy_1)(x) == g_xy_1)
    //
    val g_xy_2 = DepGraph(Set(x,y),Set(DepEdge(x,y,geq)))
    val dep_xy_2 = SCCs(g_xy_2)
    assert(dep_xy_2(x) == DepGraph(Set(x),Set()))
    assert(dep_xy_2(y) == DepGraph(Set(y),Set()))
    //
    val g_xy_3 = DepGraph(Set(x,y),Set(DepEdge(x,y,geq),DepEdge(y,x,geq)))
    val dep_xy_3 = SCCs(g_xy_3)
    assert(dep_xy_3(x) == g_xy_3)
    assert(dep_xy_3(y) == dep_xy_3(x))
    //
    //
    val g_xyz_1 = DepGraph(Set(x,z),Set())
    assert(SCCs(g_xyz_1)(x) == DepGraph(Set(x),Set()))
    assert(SCCs(g_xyz_1)(z) == DepGraph(Set(z),Set()))
    //
    val g_xyz_2 = DepGraph(Set(x,y,z),Set(DepEdge(x,y,geq)))
    val dep_xyz_2 = SCCs(g_xyz_2)
    assert(dep_xyz_2(x) == DepGraph(Set(x),Set()))
    assert(dep_xyz_2(y) == DepGraph(Set(y),Set()))
    assert(dep_xyz_2(z) == DepGraph(Set(z),Set()))
    //
    val g_xyz_3 = DepGraph(Set(x,y),Set(DepEdge(x,y,geq),DepEdge(y,x,geq)))
    val dep_xyz_3 = SCCs(g_xyz_3)
    assert(dep_xyz_3(x) == g_xy_3)
    assert(dep_xyz_3(y) == g_xy_3)
    assert(dep_xyz_3(z) == DepGraph(Set(z),Set()))
  }

  object a extends Atom
  object b extends Atom
  object c extends Atom
  object d extends Atom
  object e extends Atom
  object h extends Atom
  object i extends Atom
  object j extends Atom

  test("test 2") {
    val g1 = DepGraph(Set(a,b,c,d,e),Set(
      DepEdge(a,b,geq),
      DepEdge(b,c,geq),
      DepEdge(c,a,geq),
      DepEdge(d,e,eql),
      DepEdge(e,d,eql)))
    val dep_g1 = SCCs(g1)
    val comp_a = dep_g1(a)
    val comp_b = dep_g1(b)
    val comp_c = dep_g1(c)
    val comp_d = dep_g1(d)
    val comp_e = dep_g1(e)

    assert(comp_a == comp_b)
    assert(comp_b == comp_c)
    assert(comp_d == comp_e)

    assert(comp_a == DepGraph(Set(a,b,c),Set(DepEdge(a,b,geq),DepEdge(b,c,geq),DepEdge(c,a,geq))))
    assert(comp_c == DepGraph(Set(d,e),Set(DepEdge(d,e,eql),DepEdge(e,d,eql))))
  }

}
