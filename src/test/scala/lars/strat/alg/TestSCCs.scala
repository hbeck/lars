package lars.strat.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.graph.alg.SCCFn
import lars.strat._
import org.scalatest.FunSuite

/**
 * Created by hb on 7/11/15.
 */
class TestSCCs extends FunSuite {

  //see package object for atoms and graphs
  val fn  = SCCFn[ExtendedAtom]()

  test("test x y z") {
    assert(fn(g_x)(x) == g_x.nodes)
    //
    val dep_xy_2 = fn(g_xy_1)
    assert(dep_xy_2(x) == Set(x))
    assert(dep_xy_2(y) == Set(y))
    //
    val dep_xy_3 = fn(g_xy_2)
    assert(dep_xy_3(x) == g_xy_2.nodes)
    assert(dep_xy_3(y) == dep_xy_3(x))
    //
    //
    assert(fn(g_xz)(x) == Set(x))
    assert(fn(g_xz)(z) == Set(z))
    //
    val dep_xyz_2 = fn(g_xyz_1)
    assert(dep_xyz_2(x) == Set(x))
    assert(dep_xyz_2(y) == Set(y))
    assert(dep_xyz_2(z) == Set(z))
    //
    val dep_xyz_3 = fn(g_xyz_2)
    assert(dep_xyz_3(x) == g_xy_2.nodes)
    assert(dep_xyz_3(y) == g_xy_2.nodes)
    assert(dep_xyz_3(z) == Set(z))
  }

  test("test 2") {
    val depG = fn(g1)

    val comp_a = depG(a)
    val comp_b = depG(b)
    val comp_c = depG(c)
    val comp_d = depG(d)
    val comp_f = depG(f)

    assert(comp_a == comp_b)
    assert(comp_b == comp_c)
    assert(comp_d == comp_f)

    assert(comp_a == g(Set(a,b,c),e(a,b,geq),e(b,c,geq),e(c,a,geq)).nodes)
    assert(comp_d == g(Set(d,f),e(d,f,eql),e(f,d,eql)).nodes)
  }

  test("test 3") {
    val depG = fn(g2)

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

    assert(comp_a == g(Set(a,b,c),e(a,b,geq),e(b,c,geq),e(c,a,geq)).nodes)
    assert(comp_d == g(Set(d,f),e(d,f,eql),e(f,d,eql)).nodes)
    assert(comp_h == g(Set(h)).nodes)
    assert(comp_i == g(Set(i,j),e(i,j,geq),e(j,i,geq)).nodes)
  }

}
