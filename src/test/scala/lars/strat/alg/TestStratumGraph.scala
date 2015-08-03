package lars.strat.alg

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.graph.alg.{DepPartition, BottomUpNumbering, SCCFn}
import lars.graph.quotient.{QuotientGraph, Condensation}
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
  object f extends Atom
  object g extends Atom
  object h extends Atom
  object i extends Atom
  object j extends Atom



  test("test1"){

    val a_c = e(a,c,geq)
    val b_a = e(b,a,geq)
    val b_d = e(b,d,geq)
    val d_c = e(d,c,grt)

    val nodes = Set[ExtendedAtom](d,b,c,a)
    val edges = Set[DepEdge](a_c,b_a,b_d,d_c)

    val depGraph = DepGraph(nodes,edges)


    val quot = Condensation(depGraph).adjList

    val set_abd_c = Map(Set(c)->Set(),Set(a,b,d)->Set(Set(c)))
    val set_ac_bd = Map(Set(a,c)->Set(), Set(b,d)->Set(Set(a,c)))

    assert(quot == set_abd_c || quot == set_ac_bd)
  }

  test("test2"){

    val a_b = e(a,b,geq)
    val a_f = e(a,f,grt)
    val a_g = e(a,g,geq)
    val b_c = e(b,c,geq)
    val c_d = e(c,d,geq)
    val d_h = e(d,h,grt)
    val d_i = e(d,i,geq)
    val d_b = e(d,b,geq)
    val h_j = e(h,j,geq)
    val i_j = e(i,j,geq)
    val f_i = e(f,i,geq)
    val f_j = e(f,j,geq)
    val g_j = e(g,j,geq)



    val nodes = Set[ExtendedAtom](a,b,c,d,f,g,h,i,j)
    println(nodes)
    val edges = Set[DepEdge](a_b,a_f,a_g,b_c,c_d,d_h,d_i,d_b,h_j,i_j,f_i,f_j,g_j)

    val depGraph = DepGraph(nodes,edges)

    val quot = Condensation(depGraph).adjList
    println(quot)

/*    val set_abd_c = Map(Set(c)->Set(),Set(a,b,d)->Set(Set(c)))
    val set_ac_bd = Map(Set(a,c)->Set(), Set(b,d)->Set(Set(a,c)))

    assert(quot == set_abd_c || quot == set_ac_bd)*/
  }
}
