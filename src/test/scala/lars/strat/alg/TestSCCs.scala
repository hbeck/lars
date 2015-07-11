package lars.strat.alg

import lars.core.semantics.formulas.{ExtendedAtom, Atom}
import lars.strat._
import org.scalatest.FunSuite

/**
 * Created by hb on 7/11/15.
 */
class TestSCCs extends FunSuite {

  object x extends Atom
  object y extends Atom
  object z extends Atom
  
  def e(from:ExtendedAtom, to:ExtendedAtom, dep: Dep) = DepEdge(from,to,dep)  
  def g(s:Set[ExtendedAtom], e:DepEdge*): DepGraph = DepGraph(s,e.toSet)

  test("test x y z") {
    val g_xy_1 = g(Set(x))
    assert(SCCs(g_xy_1)(x) == g_xy_1)
    //
    val g_xy_2 = g(Set(x,y),e(x,y,geq))
    val dep_xy_2 = SCCs(g_xy_2)
    assert(dep_xy_2(x) == g(Set(x)))
    assert(dep_xy_2(y) == g(Set(y)))
    //
    val g_xy_3 = g(Set(x,y),e(x,y,geq),e(y,x,geq))
    val dep_xy_3 = SCCs(g_xy_3)
    assert(dep_xy_3(x) == g_xy_3)
    assert(dep_xy_3(y) == dep_xy_3(x))
    //
    //
    val g_xyz_1 = g(Set(x,z))
    assert(SCCs(g_xyz_1)(x) == g(Set(x)))
    assert(SCCs(g_xyz_1)(z) == g(Set(z)))
    //
    val g_xyz_2 = g(Set(x,y,z),e(x,y,geq))
    val dep_xyz_2 = SCCs(g_xyz_2)
    assert(dep_xyz_2(x) == g(Set(x)))
    assert(dep_xyz_2(y) == g(Set(y)))
    assert(dep_xyz_2(z) == g(Set(z)))
    //
    val g_xyz_3 = g(Set(x,y,z),e(x,y,geq),e(y,x,geq))
    val dep_xyz_3 = SCCs(g_xyz_3)
    assert(dep_xyz_3(x) == g_xy_3)
    assert(dep_xyz_3(y) == g_xy_3)
    assert(dep_xyz_3(z) == g(Set(z)))
  }

  object a extends Atom
  object b extends Atom
  object c extends Atom
  object d extends Atom
  object f extends Atom
  object h extends Atom
  object i extends Atom
  object j extends Atom

  val g1 = g(Set(a,b,c,d,f),e(a,b,geq),e(b,c,geq),e(c,a,geq),e(d,f,eql),e(f,d,eql))

  test("test 2") {
    val depG = SCCs(g1)

    val comp_a = depG(a)
    val comp_b = depG(b)
    val comp_c = depG(c)
    val comp_d = depG(d)
    val comp_f = depG(f)

    assert(comp_a == comp_b)
    assert(comp_b == comp_c)
    assert(comp_d == comp_f)

    assert(comp_a == g(Set(a,b,c),e(a,b,geq),e(b,c,geq),e(c,a,geq)))
    assert(comp_d == g(Set(d,f),e(d,f,eql),e(f,d,eql)))
  }

  val g2 = DepGraph(
    g1.nodes ++ Set(h,i,j),
    g1.edges ++ Set(e(a,h,grt),e(c,i,grt),e(f,j,grt),e(i,j,geq),e(j,i,geq)))

  test("test 3") {
    val depG = SCCs(g2)

    val comp_a = depG(a)
    val comp_b = depG(b)
    val comp_c = depG(c)
    val comp_d = depG(d)
    val comp_f = depG(f)
    val comp_h = depG(h)
    val comp_i = depG(i)
    val comp_j = depG(j)

    assert(comp_a == comp_b)
    assert(comp_b == comp_c)
    assert(comp_d == comp_f)
    assert(comp_i == comp_j)
    //
    assert(comp_a != comp_h)
    assert(comp_a != comp_d)
    assert(comp_a != comp_i)
    //
    assert(comp_d != comp_h)
    assert(comp_a != comp_i)
    //
    assert(comp_h != comp_i)

    assert(comp_a == g(Set(a,b,c),e(a,b,geq),e(b,c,geq),e(c,a,geq)))
    assert(comp_d == g(Set(d,f),e(d,f,eql),e(f,d,eql)))
    assert(comp_h == g(Set(h)))
    assert(comp_i == g(Set(i,j),e(i,j,geq),e(j,i,geq)))
  }

}
