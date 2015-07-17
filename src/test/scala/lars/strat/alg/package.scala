package lars.strat

import lars.core.semantics.formulas.{Atom, ExtendedAtom}

/**
 * Created by hb on 7/11/15.
 */
package object alg {

  def e(from:ExtendedAtom, to:ExtendedAtom, dep: Dep) = DepEdge(from,to,dep)
  def g(s:Set[ExtendedAtom], e:DepEdge*): DepGraph = DepGraph(s,e.toSet)

  //

  object x extends Atom
  object y extends Atom
  object z extends Atom
  object a extends Atom
  object b extends Atom
  object c extends Atom
  object d extends Atom
  object f extends Atom
  object h extends Atom
  object i extends Atom
  object j extends Atom

  val g_x = g(Set(x))
  val g_xy_1 = g(Set(x,y),e(x,y,geq))
  val g_xy_2 = g(Set(x,y),e(x,y,geq),e(y,x,geq))
  val g_xz = g(Set(x,z))
  val g_xyz_1 = g(Set(x,y,z),e(x,y,geq))
  val g_xyz_2 = g(Set(x,y,z),e(x,y,geq),e(y,x,geq))

  val g1 = g(Set(a,b,c,d,f),e(a,b,geq),e(b,c,geq),e(c,a,geq),e(d,f,eql),e(f,d,eql))

  val g2 = DepGraph(
    g1.nodes ++ Set(h,i,j),
    g1.depEdges ++ Set(e(a,h,grt),e(c,i,grt),e(f,j,grt),e(i,j,geq),e(j,i,geq)))

}
