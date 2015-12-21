package lars.strat

import lars.core.semantics.formulas.{Atom, ExtendedAtom}

/**
 * Created by hb on 7/11/15.
 */
package object alg {

  def e(from:ExtendedAtom, to:ExtendedAtom, dep: Dependency) = DepEdge(from,to,dep)
  def g(nodes:Set[ExtendedAtom], e:DepEdge[ExtendedAtom]*): DepGraph[ExtendedAtom] = DepGraph(nodes,e.toSet)
  def gs(nodes:Set[ExtendedAtom], edges: Set[DepEdge[ExtendedAtom]]): DepGraph[ExtendedAtom] = DepGraph(nodes,edges)

  //

  object x extends Atom
  object y extends Atom
  object z extends Atom

  object a extends Atom
  object a1 extends Atom
  object a2 extends Atom
  object a3 extends Atom
  object b extends Atom
  object c extends Atom
  object d extends Atom
  object d1 extends Atom
  object d2 extends Atom
  object f extends Atom
  object h extends Atom
  object i extends Atom
  object j extends Atom

  object x1 extends Atom
  object x2 extends Atom
  object x3 extends Atom
  object y1 extends Atom
  object y2 extends Atom
  object z1 extends Atom
  object z2 extends Atom
  object w1 extends Atom
  object w2 extends Atom
  object w3 extends Atom
  object wa extends Atom
  object wb extends Atom
  object wc extends Atom
  object wd extends Atom
  object wf extends Atom
  object wh extends Atom
  object wx extends Atom
  object wy extends Atom
  object wd1 extends Atom


  val g_x = g(Set(x))
  val g_xy_1 = g(Set(x,y),e(x,y,geq))
  val g_xy_2 = g(Set(x,y),e(x,y,geq),e(y,x,geq))
  val g_xz = g(Set(x,z))
  val g_xyz_1 = g(Set(x,y,z),e(x,y,geq))
  val g_xyz_2 = g(Set(x,y,z),e(x,y,geq),e(y,x,geq))

  val g1_edges = Set(e(a,b,geq),e(b,c,geq),e(c,a,geq),e(d,f,eql),e(f,d,eql))
  val g1 = gs(Set(a,b,c,d,f),g1_edges)

  val g2 = DepGraph(
    g1.nodes ++ Set(h,i,j),
    g1_edges ++ Set(e(a,h,grt),e(c,i,grt),e(f,j,grt),e(i,j,geq),e(j,i,geq)))

}
